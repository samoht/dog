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

  let show fmt =
    Printf.ksprintf (fun str -> Printf.printf "%s\n" (green_s str)) fmt

  let merge_ignore p ~old:_ _ _ =
    show "IGNORE  %s" (path p);
    ok None

  let merge_replace p ~old:_ _ y =
    show "REPLACE %s" (path p);
    ok y

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

let timestamp () =
  let ts = Unix.gettimeofday() in
  let tm = Unix.localtime ts in
  let us, _s = modf ts in
  let ts =
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d.%03d "
    (1900 + tm.Unix.tm_year)
    (1    + tm.Unix.tm_mon)
    (tm.Unix.tm_mday)
    (tm.Unix.tm_hour)
    (tm.Unix.tm_min)
    (tm.Unix.tm_sec)
    (int_of_float (1_000. *. us))
  in
  file (Cstruct.of_string ts)

let str t fmt = Printf.ksprintf (fun str -> t str) fmt

let listen ~root =
  let config =
    Irmin_git.config ~root ~bare:false ~head:Git.Reference.master ()
  in
  Irmin.create base_store config task >>= fun t ->
  Irmin.tags (t "Getting tags") >>= fun clients ->
  printf "Current clients: %s\n%!" (String.concat " " clients);
  with_store ~root (fun merges t _ ->
      let module Conf = struct let merges = merges end in
      let module File = File (Conf) in
      let module Server = Irmin.Basic (Irmin_git.FS) (File) in
      let module HTTP = Irmin_http_server.Make(Server) in
      Irmin.update (t "Starting the server") [".started"] (timestamp ())
      >>= fun () ->
      let listen () =
        Server.create config task >>= fun s ->
        HTTP.listen (s "Listen") (Uri.of_string "http://localhost:8080")
      in
      let clients_ref = ref [] in
      let watch client =
        let merge head =
          match head with
          | None   -> (* FIXME: handle client erasure *) Lwt.return_unit
          | Some h ->
            Irmin.merge_head (str t "Merging %s's changes" client) ~n:1 h
            >>= function
            | `Ok ()      -> Lwt.return_unit
            | `Conflict c ->
              Log.error "Cannot merge %s: %s" client c;
              Lwt.return_unit
        in
        if List.mem client !clients_ref then Lwt.return_unit
        else (
          printf "new client: %s\n!" client;
          clients_ref := client :: !clients_ref;

          Irmin.of_tag base_store config task client >>= fun c ->
          Irmin.head (c "head") >>= fun head ->
          merge head >>= fun () ->

          let stream =
            Irmin.watch_head (str c "Watching changes for client %s" client) []
          in
          Lwt_stream.iter_s (fun (_path, head) ->
              printf "got some changes for %s\n%!" client;
              merge head
            ) stream
        )
      in
      let watch_new () =
        let stream = Irmin.watch_tags (t "Watching tags") in
        Lwt_stream.iter (fun (client, value) ->
            printf "got some global changes (%s)\n%!" client;
            match value with
            | None   -> (* FIXME: stop watchers for this client *) ()
            | Some _ -> Lwt.async (fun () -> watch client)
          ) stream

      in
      install_dir_polling_listener 1.;
      Lwt.join (listen () :: watch_new () :: List.map watch clients)
    )
