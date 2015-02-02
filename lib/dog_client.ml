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

open Irmin_unix

let (>>=) = Lwt.bind

let init ~root name =
  let head = Git.Reference.of_raw ("refs/heads/" ^ name) in
  let config = Irmin_git.config ~root ~bare:false ~head () in
  Irmin.of_tag Dog_misc.base_store config Dog_misc.task name >>= fun t ->
  Irmin.head (t "head") >>= function
  | Some _ -> Lwt.return_unit
  | None   ->
    Irmin.update (t "Initial commit") [".init"] (Dog_misc.timestamp ())

let remote_store =
  Irmin.basic (module Irmin_http.Make) (module Irmin.Contents.String)

let chdir dir = Unix.handle_unix_error Unix.chdir dir

let in_dir dir fn =
  let reset_cwd =
    let cwd = Unix.handle_unix_error Unix.getcwd () in
    fun () -> chdir cwd in
  chdir dir;
  try
    let r = fn () in
    reset_cwd ();
    r
  with e ->
    reset_cwd ();
    raise e

let (/) x y = x @ [y]

let list kind dir =
  in_dir dir (fun () ->
      let d = Sys.readdir (Sys.getcwd ()) in
      let d = Array.to_list d in
      List.filter kind d
    )

let files =
  list (fun f -> try not (Sys.is_directory f) with Sys_error _ -> true)

let directories =
  list (fun f -> try Sys.is_directory f with Sys_error _ -> false)

let rec_files ?(keep=fun _ -> true) root =
  let rec aux accu dir =
    let path = Dog_misc.path (root :: dir) in
    let d =
      directories path
      |> List.filter keep
      |> List.map ((/) dir)
    in
    let f =
      files path
      |> List.filter keep
      |> List.map ((/) dir)
    in
    List.fold_left aux (f @ accu) d in
  aux [] []

let keep = function ".git" -> false | _ -> true

let of_path p =
    let file = Dog_misc.path p in
    let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
    let ba = Bigarray.(Array1.map_file fd char c_layout false (-1)) in
    Unix.close fd;
    Cstruct.to_string (Cstruct.of_bigarray ba)

let update_files files view =
  Lwt_list.iter_s (fun path ->
      Irmin.update view path (of_path path)
    ) files

let push ~root ~msg ?watch server =
  let config = Irmin_http.config server in
  Dog_misc.(mk_store base_store ~root) >>= fun t ->
  let tag = Irmin.tag_exn (t "tag") in
  Irmin.of_tag remote_store config Dog_misc.task tag >>= fun remote ->
  let rec aux () =
    let files = rec_files ~keep root in
    Irmin.with_hrw_view (t msg) `Update (update_files files) >>=
    Irmin.Merge.exn >>= fun () ->
    let remote = Irmin.remote_basic (remote "dog push") in
    Irmin.push_exn (t msg) remote >>= fun () ->
    match watch with
    | None   -> Lwt.return_unit
    | Some d ->
      Lwt_unix.sleep d >>= fun () ->
      aux ()
  in
  aux ()
