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

open Dog_misc
open Lwt.Infix

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


let read_file p =
  let file = Dog_misc.path p in
  let ic = open_in file in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  buf

module View = Irmin.View(Store)

let update_files t files =
  View.of_path t [] >>= fun view ->
  Lwt_list.iter_s (fun path ->
      View.update view path (read_file path)
    ) files
  >>= fun () ->
  View.update_path t [] view

let keep = function ".git" -> false | _ -> true

let cmd ~root ?watch name remote =
  let head = Git.Reference.of_raw ("refs/heads/" ^ name) in
  let config = Irmin_git.config ~root ~bare:false ~head () in
  Store.Repo.create config >>= fun repo ->
  Store.of_branch_id task name repo >>= fun t ->
  begin Store.head (t "head") >>= function
    | Some _ -> Lwt.return_unit
    | None   -> Store.update (t "Initial commit") [".init"] (timestamp ())
  end >>= fun () ->
  let rec aux () =
    let files = rec_files ~keep root in
    update_files (t "update files") files >>= fun () ->
    git_push ~root ~url:remote ~branch:name;
    match watch with
    | None   -> Lwt.return_unit
    | Some d ->
      Lwt_unix.sleep d >>= fun () ->
      aux ()
  in
  aux ()
