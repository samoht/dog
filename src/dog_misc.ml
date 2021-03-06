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

let task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner =
    let tmp = Filename.temp_file "git-config" "user-name" in
    let i = Sys.command (sprintf "git config user.name > %s" tmp) in
    let name =
      if i <> 0 then Printf.sprintf "%s.[%d]" (Unix.gethostname()) (Unix.getpid())
      else
        let ic = open_in tmp in
        let line = input_line ic in
        close_in ic;
        line
    in
    Sys.remove tmp;
    name
  in
  Irmin.Task.create ~date ~owner msg

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

let git_push ~root ?(force=true) ?(branch="master") url =
  (* FIXME: to replace by ocaml-git push at one point *)
  let redirect = match Logs.level () with
    | Some (Logs.Debug | Logs.Info) -> ""
    | _ -> " > /dev/null 2>&1"
  in
  let force = if force then " --force" else "" in
  let cmd =
    Printf.sprintf "cd %s && git push %s %s%s%s" root url branch force redirect
  in
  let i = Sys.command cmd in
  if i <> 0 then failwith "git push failed";
  Lwt.return_unit

module Store = Irmin_unix.Irmin_git.FS
    (Irmin.Contents.String) (Irmin.Ref.String) (Irmin.Hash.SHA1)
