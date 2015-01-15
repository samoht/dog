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

let (>>=) = Lwt.bind

let rec pretty_list ?(last="and") = function
  | []    -> ""
  | [a]   -> a
  | [a;b] -> Printf.sprintf "%s %s %s" a last b
  | h::t  -> Printf.sprintf "%s, %s" h (pretty_list t)

let string_chop_prefix t ~prefix =
  let lt = String.length t in
  let lp = String.length prefix in
  if lt < lp then None else
    let p = String.sub t 0 lp in
    if String.compare p prefix <> 0 then None
    else Some (String.sub t lp (lt - lp))

let ppf f oc =
  let ppf = Format.formatter_of_out_channel oc in
  f ppf

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
  sprintf "{ %s }" (pretty_list (List.map string_of_merge all))

let merge_of_string = function
  | "ignore" -> Some `Ignore
  | "replace" -> Some `Replace
  | "set" -> Some `Set
  | "append" -> Some `Append
  | "jsonx" -> Some `Jsonx
  | s -> None

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
  try snd (List.find (fun (pat, _) -> check pat file) merges)
  with Not_found -> `Ignore

type file = {
  digest: Digest.t;
  buf: Cstruct.t;
}

let digest buf = Digest.string (Cstruct.to_string buf)
let file buf = { buf; digest = digest buf }

module type CONF = sig
  val merges: unit -> merges Lwt.t
end

module Lines = struct

  let list buf =
    let buf = Mstruct.of_cstruct buf in
    let rec aux acc =
      match Mstruct.get_string_delim buf '\n' with
      | None -> List.rev acc
      | Some l -> aux (l :: acc)
    in
    aux []

  module StringSet = struct
    include Set.Make(String)
    let of_list l = List.fold_left (fun s x -> add x s) empty l
  end

  let set buf = StringSet.of_list (list buf)

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

  let merge_ignore ~old:_ _ _ = ok None
  let merge_replace ~old:_ _ y = ok y
  let merge_set ~old:_ _ _= failwith "TODO"
  let merge_append ~old:_ _ _ = failwith "TODO"
  let merge_jsonx ~old:_ _ _ = failwith "TODO"

  let merge path ~old x y =
    (* FIXME: cache the call? *)
    Conf.merges () >>= fun merges ->
    let merge =  merge merges (String.concat "/" path) in
    match merge with
    | `Ignore -> merge_ignore ~old x y
    | `Replace -> merge_replace ~old x y
    | `Set -> merge_set ~old x y
    | `Append -> merge_append ~old x y
    | `Jsonx -> merge_jsonx ~old x y

end

type t = (string list, file) Irmin.t

let dot_merges = ".merges"
let default_merges: merges = [ pattern_exn dot_merges, `Set ]
let dot_merges_file = [dot_merges]

(* Used for read the config file *)
let base_store =
  Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String)

let mk_store store ~root =
  Git_unix.FS.create ~root () >>= fun g ->
  Git_unix.FS.read_head g >>= function
  | None  -> failwith (sprintf "%s is not a valid Git repository" root)
  | Some (Git.Reference.SHA _) ->
    failwith (sprintf "%s does not have a valid branch" root)
  | Some (Git.Reference.Ref r) ->
    let name =
      let head = Git.Reference.to_raw r in
      match string_chop_prefix head ~prefix:"refs/heads/" with
      | None   -> failwith (sprintf "%s is not a valid reference" head)
      | Some n -> n
    in
    let config = Irmin_git.config ~root ~bare:false ~head:r () in
    Irmin.of_tag store config task name

let raw_store = mk_store base_store

let with_store ~root msg fn =
  raw_store ~root >>= fun t ->
  let merges () =
    Irmin.read (t "Reading .merges") dot_merges_file >>= function
    | None -> failwith (sprintf "%s is not a valid Dog repository." root)
    | Some buf -> Lwt.return (merges_of_string buf)
  in
  let module Conf = struct let merges = merges end in
  let module File = File (Conf) in
  let store = Irmin.basic (module Irmin_git.FS) (module File) in
  mk_store store ~root >>= fun t ->
  fn Conf.merges (t msg)

let task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner =
    let tmp = Filename.temp_file "git-config" "user-name" in
    let i = Sys.command (sprintf "git config user.name > %s" tmp) in
    let name =
      if i <> 0 then Printf.sprintf "%s.[%d]" (Unix.gethostname()) (Unix.getpid())
      else
        let ic = open_in tmp in
        input_line ic
    in
    Sys.remove tmp;
    name
  in
  Irmin.Task.create ~date ~owner msg

let init ~root name =
  let head = Git.Reference.of_raw ("refs/heads/" ^ name) in
  let config = Irmin_git.config ~root ~bare:false ~head () in
  Irmin.of_tag base_store config task name >>= fun t ->
  let merges = string_of_merges default_merges in
  Irmin.update (t "Creating .merges.") dot_merges_file merges

let remote_store =
  Irmin.basic (module Irmin_http.Make) (module Irmin.Contents.String)

let push ~root ~msg server =
  with_store ~root "dog push" (fun _ t ->
      let config = Irmin_http.config server in
      Irmin.create remote_store config task >>= fun remote ->
      let remote = Irmin.remote_basic (remote msg) in
      Irmin.push_exn t remote
    )

(* FIXME: replace that by some Irmin watches on new tags (which
   doesn't exist atm) *)
let dot_clients_file = [".clients"]
let clients_of_string buf =
  let buf = Mstruct.of_string buf in
  let rec aux acc =
    match Mstruct.get_string_delim buf '\n' with
    | None -> List.rev acc
    | Some l -> aux (l :: acc)
  in
  aux []

let str t fmt = Printf.ksprintf (fun str -> t str) fmt

let listen ~root =
  let config = Irmin_git.config ~root ~bare:false () in
  Irmin.create base_store config task >>= fun t ->
  Irmin.read (t "Loading .clients.") dot_clients_file >>= fun buf ->
  let clients = match buf with
    | None   -> []
    | Some s -> clients_of_string s
  in
  with_store ~root "dog listen" (fun merges _ ->
      let module Conf = struct let merges = merges end in
      let module File = File (Conf) in
      let module Server = Irmin.Basic (Irmin_git.FS) (File) in
      let module HTTP = Irmin_http_server.Make(Server) in
      Server.create config task >>= fun t ->
      let listen () =
        HTTP.listen (t "Listen") (Uri.of_string "http://localhost:1234")
      in
      let watch client =
        Irmin.of_tag base_store config task client >>= fun c ->
        let stream =
          Irmin.watch_head (str c "Watching changes for client %s" client) []
        in
        Lwt_stream.iter_s (fun (_path, head) ->
            Server.merge_head (str t "Merging %s's changes" client) head
            >>= function
            | `Ok () -> Lwt.return_unit
            | `Conflict c ->
              Log.error "Cannot merge %s: %s" client c;
              Lwt.return_unit
          ) stream
      in
      Lwt.join (listen () :: List.map watch clients)
    )
