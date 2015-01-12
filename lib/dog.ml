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

let ppf f oc =
  let ppf = Format.formatter_of_out_channel oc in
  f ppf

type merge =
  [ `Ignore
  | `Replace
  | `Line_set
  | `Line_append
  | `Jsonx ]

let string_of_merge = function
  | `Ignore -> "ignore"
  | `Replace -> "replace"
  | `Line_set -> "line-set"
  | `Line_append -> "line-append"
  | `Jsonx -> "jsonx"

let merge_of_string = function
  | "ignore" -> Some `Ignore
  | "replace" -> Some `Replace
  | "line-set" -> Some `Line_set
  | "line-append" -> Some `Line_append
  | "jsonx" -> Some `Jsonx
  | s -> None

let merge_of_string_exn x =
  match merge_of_string x with
  | Some x -> x
  | None -> failwith (sprintf "%s is not a valid merge strategies." x)

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

type conf = {
  client: string;
  server: Uri.t;
  merges: (pattern * merge) list;
}

let toml_of_merge (p, m) =
  Toml.Value.Of.Array.string [string_of_pattern p; string_of_merge m]

let merge_of_toml t =
  match Toml.Value.To.Array.string t with
  | []     -> failwith "empty merge pattern"
  | [p; m] -> (pattern_exn p, merge_of_string_exn m)
  | p :: _ -> failwith (sprintf "%s has too many merge stategies" p)

let toml_of_merges ms =
  List.map toml_of_merge ms
  |> Toml.Value.Of.Array.array
  |> Toml.Value.Of.array

let merges_of_toml t =
  Log.debug "merges: %a" (ppf Toml.Printer.value) t;
  Toml.Value.To.array t
  |> Toml.Value.To.Array.array
  |> List.map merge_of_toml

let toml_of_conf conf =
  List.fold_left (fun map (k, v) ->
      Toml.Table.add (Toml.key k) v map
    ) Toml.Table.empty [
    "client", Toml.Value.Of.string conf.client;
    "server", Toml.Value.Of.string (Uri.to_string conf.server);
    "merges", toml_of_merges conf.merges;
  ]

let conf_of_toml t =
  let read key fn =
    try Toml.Table.find (Toml.key key) t |> fn
    with Not_found -> failwith (sprintf "conf_of_toml: missing %s" key)
  in
  let client = read "client" Toml.Value.To.string in
  let server = read "server" (fun x -> Uri.of_string (Toml.Value.To.string x)) in
  let merges = read "merges" merges_of_toml in
  { client; server; merges }

let string_of_conf conf =
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  Toml.Printer.table ppf (toml_of_conf conf);
  Buffer.contents buf

let conf_of_string buf =
  Toml.Parser.from_string buf
  |> conf_of_toml

let conf ~client ~server ~merges =
  { client; server; merges }

let client c = c.client
let server c = c.server
let merges c = c.merges
let merge c file =
  try snd (List.find (fun (pat, _) -> check pat file) c.merges)
  with Not_found -> `Ignore

type file = {
  conf: conf;
  digest: Digest.t;
  buf: Cstruct.t;
}

let digest buf = Digest.string (Cstruct.to_string buf)
let file conf buf = { buf; digest = digest buf; conf }

module type CONF = sig
  val v: conf
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

  let read buf = file Conf.v (Mstruct.to_cstruct buf)

  let to_json t = Ezjsonm.encode_string (Cstruct.to_string t.buf)
  let of_json j = file Conf.v (Cstruct.of_string (Ezjsonm.decode_string_exn j))

  open Irmin.Merge.OP

  let merge_ignore ~old:_ _ _ = ok None
  let merge_replace ~old:_ _ y = ok y
  let merge_line_set ~old:_ _ _= failwith "TODO"
  let merge_line_append ~old:_ _ _ = failwith "TODO"
  let merge_jsonx ~old:_ _ _ = failwith "TODO"

  let merge (path:Path.t): t option Irmin.Merge.t =
    let merge =  merge Conf.v (String.concat "/" path) in
    match merge with
    | `Ignore -> merge_ignore
    | `Replace -> merge_replace
    | `Line_set -> merge_line_set
    | `Line_append -> merge_line_append
    | `Jsonx -> merge_jsonx

end

type t = (string list, file) Irmin.t

let conf_path = [".dog"]

let (>>=) = Lwt.bind

(* Used for read the config file *)
let base_store = Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String)

let with_store ~root msg fn =
  let config = Irmin_git.config ~root ~bare:false () in
  Irmin.create base_store config task >>= fun t ->
  Irmin.read (t "Reading the config file") conf_path >>= function
  | None -> failwith (sprintf "%s is not a valid Dog repository." root)
  | Some conf ->
    let module Conf = struct let v = conf_of_string conf end in
    let module File = File (Conf) in
    let store = Irmin.basic (module Irmin_git.FS) (module File) in
    Irmin.create store config task >>= fun t ->
    fn (t msg)

let init ~root conf =
  let config = Irmin_git.config ~root ~bare:false () in
  Irmin.create base_store config task >>= fun t ->
  let conf = string_of_conf conf in
  Irmin.update (t "Writing the configuration file.") conf_path conf

let list_merges ~root = failwith "TODO"
let add_merge ~root pattern merge = failwith "TODO"
let remove_merge ~root pattern = failwith "TODO"
let push ~root msg = failwith "TODO"
let listen ~root = failwith "TODO"
