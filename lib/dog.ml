(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Printf
open Irmin_unix
open Dog_misc
let (>>=) = Lwt.bind

type merge =
  [ `Ignore
  | `Replace
  | `Set
  | `Append
  | `Jsonx ]

let string_of_merge = function
  | `Ignore -> "ignore"
  | `Replace -> "replace"
  | `Set -> "set"
  | `Append -> "append"
  | `Jsonx -> "jsonx"

let all_merges =
  let all = [ `Ignore; `Replace; `Set; `Append; `Jsonx ] in
  sprintf "{ %s }" (Dog_misc.pretty_list (List.map string_of_merge all))

let merge_of_string = function
  | "ignore" -> Some `Ignore
  | "replace" -> Some `Replace
  | "set" -> Some `Set
  | "append" -> Some `Append
  | "jsonx" -> Some `Jsonx
  | _ -> None

let merge_of_string_exn x =
  match merge_of_string x with
  | Some x -> x
  | None ->
    failwith (sprintf "%s is not a valid merge strategies. \
                       Valid stategies are: %s" x all_merges)

type pattern = string * (string -> bool)

let pattern_err pat =
  `Error (sprintf "%s is not a valid pattern" pat)

let string_of_pattern (p, _) = p

let pattern pat =
  match
    (try `Ok (Re.compile (Re.no_case (Re_glob.globx pat)))
     with Re_glob.Parse_error -> pattern_err pat)
  with
  | `Error e -> `Error e
  | `Ok re   -> `Ok (pat, fun x -> Re.execp re x)

let compare_pattern (x, _) (y, _) = String.compare x y

let pattern_exn pat =
  match pattern pat with
  | `Ok x    -> x
  | `Error e -> raise (Failure e)

let check (_, f) x = f x

type merges = (pattern * merge) list

let string_of_pm (p, m) =
  Printf.sprintf "%s %s" (string_of_pattern p) (string_of_merge m)

let pm_of_string_exn s =
  if String.length s > 0 && s.[0] = '#' then
    (* discard comments *)
    None
  else try
      let s = String.trim s in
      let i = String.rindex s ' ' in
      let p = String.sub s 0 i in
      let m = String.sub s (i+1) (String.length s - i - 1) in
      Some (pattern_exn p, merge_of_string_exn m)
    with Not_found ->
      failwith (sprintf "%s is not a valid merge strategy line." s)

let string_of_merges ms =
  let buf = Buffer.create 1025 in
  List.iter (fun pm ->
      Buffer.add_string buf (string_of_pm pm);
      Buffer.add_char buf '\n';
    ) ms;
  Buffer.contents buf

let merges_of_string str =
  let buf = Mstruct.of_string str in
  let rec aux acc =
    match Mstruct.get_string_delim buf '\n' with
    | None   -> List.rev acc
    | Some l ->
      match pm_of_string_exn l with
      | None   -> aux acc
      | Some s -> aux (s :: acc)
  in
  aux []

let merge merges file =
  let file = path file in
  try snd (List.find (fun (pat, _) -> check pat file) merges)
  with Not_found -> `Replace

type file = {
  digest: Digest.t;
  buf: Cstruct.t;
}

let digest buf = Digest.string (Cstruct.to_string buf)
let file buf = { buf; digest = digest buf }

module type CONF = sig
  val merges: unit -> merges Lwt.t
end

let show fmt =
  Printf.ksprintf (fun str -> Printf.printf "%s\n%!" str) fmt

module File (Conf: CONF) = struct

  module Path = Irmin.Path.String_list

  type t = file

  let equal x y = x.digest = y.digest
  let compare x y = Digest.compare x.digest y.digest
  let hash x = Hashtbl.hash x
  let size_of t = Cstruct.len t.buf

  let write t buf =
    let len = Cstruct.len t.buf in
    Cstruct.blit t.buf 0 buf 0 len;
    Cstruct.shift buf len

  let read buf = file (Mstruct.to_cstruct buf)

  (* FIXME: cut lines? *)
  let to_json t = Ezjsonm.encode_string (Cstruct.to_string t.buf)
  let of_json j = file (Cstruct.of_string (Ezjsonm.decode_string_exn j))

  open Irmin.Merge.OP

  let merge_ignore p ~old:_ _ _ =
    show "%s  %s" (green_s "IGNORE") (path p);
    ok None

  let pr = function
    | None   -> "<none>"
    | Some x -> Cstruct.to_string x.buf

  let merge_replace p ~old:_ old_x new_x =
    show "%s %s old:%s new:%s" (green_s "REPLACE") (path p) (pr old_x) (pr new_x);
    ok new_x

  let merge_set _ ~old:_ _ _= failwith "TODO"
  let merge_append _ ~old:_ _ _ = failwith "TODO"
  let merge_jsonx _ ~old:_ _ _ = failwith "TODO"

  let merge path ~old x y =
    (* FIXME: cache the call? *)
    Conf.merges () >>= fun merges ->
    let merge =  merge merges path in
    match merge with
    | `Ignore -> merge_ignore path ~old x y
    | `Replace -> merge_replace path ~old x y
    | `Set -> merge_set path ~old x y
    | `Append -> merge_append path ~old x y
    | `Jsonx -> merge_jsonx path ~old x y

end

type t = ([`BC], path, file) Irmin.t

let dot_merge = ".merge"
let default_merges: merges = [ pattern_exn dot_merge, `Set ]
let dot_merge_file = [dot_merge]

let raw_store = Dog_misc.mk_store base_store

let with_store ~root fn =
  raw_store ~root >>= fun t ->
  let merges () =
    Irmin.read (t "Reading .merge") dot_merge_file >>= function
    | None     -> Lwt.return []
    | Some buf -> Lwt.return (merges_of_string buf)
  in
  let module Conf = struct let merges = merges end in
  let module File = File (Conf) in
  let store = Irmin.basic (module Irmin_git.FS) (module File) in
  mk_store store ~root >>= fun t ->
  let tag = Irmin.tag_exn (t "Getting the branch name") in
  fn Conf.merges t tag


let str t fmt = Printf.ksprintf (fun str -> t str) fmt

module type S = Irmin.BASIC with type key = string list
                             and type value = file

let config ~root =
  Irmin_git.config ~root ~bare:false ~head:Git.Reference.master ()

let merge_subtree t config client =
  let (module M: S) = Irmin.impl (t "") in
  let module V = Irmin.View(M) in
  M.of_tag config task client >>= fun t ->
  V.of_path (t "Create view") [] >>= fun view ->
  M.create config task >>= fun master ->
  V.merge_path (str master "Merging %s's changes" client) ~n:1 [client] view

module StringSet = Set.Make(String)

let listen ~root =
  let config = config ~root in
  Irmin.create base_store config task >>= fun t ->
  Irmin.tags (t "Getting tags") >>= fun clients ->
  let clients = List.filter ((<>)"master") clients in
  let () = match clients with
    | []  -> ()
    | [c] -> show "Existing client: %s" c
    | _   -> show "Existing client(s): %s\n%!"
               (String.concat " " (List.map Dog_misc.blue_s clients))
  in
  with_store ~root (fun merges t _ ->
      let module Conf = struct let merges = merges end in
      let module File = File (Conf) in
      let module Server = Irmin.Basic (Irmin_git.FS) (File) in
      let module HTTP = Irmin_http_server.Make(Server) in

      let ts = file (Cstruct.of_string (Dog_misc.timestamp ())) in
      Irmin.update (t "Starting the server") [".started"] ts >>= fun () ->

      let clients = ref StringSet.empty in
      let merge_hooks = ref [] in

      let watch client =
        if client = "master" || StringSet.mem client !clients then
          Lwt.return_unit
        else (
          show "Listening to a new client: %s." (Dog_misc.blue_s client);
          clients := StringSet.add client !clients;
          let merge () =
            merge_subtree t config client >>= function
            | `Ok ()      -> Lwt.return_unit
            | `Conflict c ->
              Log.error "Cannot merge %s: %s" client c;
              Lwt.return_unit
          in
          merge () >>= fun () ->
          merge_hooks := (client, merge) :: !merge_hooks;
          Lwt.return_unit
        )
      in

      let unwatch client =
        if client = "master" || not (StringSet.mem client !clients) then
          ()
        else (
          show "Stop listening to %s." (Dog_misc.blue_s client);
          clients := StringSet.remove client !clients;
          merge_hooks := List.filter (fun (c,_) -> c <> client) !merge_hooks;
        )
      in

      let watch_new () =
        Irmin.tags (t "tags") >>= fun current_clients ->
        let current_clients = StringSet.of_list current_clients in
        let new_clients = StringSet.diff current_clients !clients in
        let del_clients = StringSet.diff !clients current_clients in
        StringSet.iter unwatch del_clients;
        Lwt_list.iter_s watch (StringSet.elements new_clients)
      in

      let hooks = {
        Irmin_http_server.update = fun () ->
          watch_new () >>= fun () ->
          Lwt_list.iter_s (fun (_, m) -> m ()) !merge_hooks
      } in

      install_dir_polling_listener 1.;
      Server.create config task >>= fun s ->
      HTTP.listen (s "Listen") ~hooks (Uri.of_string "http://localhost:8080")
    )
