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

let () =
  let debug = try match Sys.getenv "DOGDEBUG" with
    | "" -> false
    | _  -> true
    with Not_found ->
      false
  in
  if debug then Log.set_log_level Log.DEBUG

let (>>=) = Lwt.bind

let red fmt = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt = sprintf ("\027[36m"^^fmt^^"\027[m")

let red_s = red "%s"
let green_s = green "%s"
let yellow_s = yellow "%s"
let blue_s = blue "%s"

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

(* Used for read the config file *)
let base_store =
  let module C = struct
    include Irmin.Contents.String
    let merge _ ~old:_ _ _ = assert false
  end in
  Irmin.basic (module Irmin_git.FS) (module C)

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

type path = string list
let path l = String.concat "/" l

let timestamp () =
  let ts = Unix.gettimeofday() in
  let tm = Unix.localtime ts in
  let us, _s = modf ts in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d.%03d "
    (1900 + tm.Unix.tm_year)
    (1    + tm.Unix.tm_mon)
    (tm.Unix.tm_mday)
    (tm.Unix.tm_hour)
    (tm.Unix.tm_min)
    (tm.Unix.tm_sec)
    (int_of_float (1_000. *. us))
