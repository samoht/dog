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

module StringSet = Set.Make(String)
module View = Irmin.View(Store)
module Sync = Irmin.Sync(Store)

let str t fmt = Printf.ksprintf (fun str -> t str) fmt

let show fmt =
  Printf.ksprintf (fun str -> Printf.printf "%s\n%!" str) fmt

let config ~root =
  Irmin_git.config ~root ~bare:false ~head:Git.Reference.master ()

let merge_subtree repo client =
  Store.of_branch_id task client repo >>= fun t ->
  View.of_path (t "Create view") [] >>= fun view ->
  Store.master task repo >>= fun master ->
  View.merge_path (str master "Merging %s's changes" client) ~n:1 [client] view

let cmd ~root =
  Store.Repo.create (config ~root) >>= fun repo ->
  Store.master task repo >>= fun t ->
  Store.Repo.branches repo >>= fun clients ->
  let clients = List.filter ((<>)"master") clients in
  let () = match clients with
    | []  -> ()
    | [c] -> show "Existing client: %s" c
    | _   -> show "Existing client(s): %s\n%!"
               (String.concat " " (List.map Dog_misc.blue_s clients))
  in
  let ts = Dog_misc.timestamp () in
  Store.update (t "Starting the server") [".started"] ts >>= fun () ->
  let clients = ref StringSet.empty in
  let merge_hooks = ref [] in

  let watch client =
    if client = "master" || StringSet.mem client !clients then
      Lwt.return_unit
    else (
      show "Listening to a new client: %s." (Dog_misc.blue_s client);
      clients := StringSet.add client !clients;
      let merge () =
        merge_subtree repo client >>= function
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
    Store.Repo.branches repo >>= fun current_clients ->
    let current_clients = StringSet.of_list current_clients in
    let new_clients = StringSet.diff current_clients !clients in
    let del_clients = StringSet.diff !clients current_clients in
    StringSet.iter unwatch del_clients;
    Lwt_list.iter_s watch (StringSet.elements new_clients)
  in

  Irmin_unix.install_dir_polling_listener 1.;
  let _unwatch =
    Store.Repo.watch_branches repo (fun _ _ ->
        watch_new () >>= fun () ->
        Lwt_list.iter_s (fun (_, m) -> m ()) !merge_hooks
      ) in

  Lwt.return ()
