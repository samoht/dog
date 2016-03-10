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

let show fmt =
  Printf.ksprintf (fun str -> Printf.printf "%s\n%!" str) fmt

let config ~root =
  Irmin_git.config ~root ~bare:false ~head:Git.Reference.master ()

let update_subtree repo client =
  Store.of_branch_id task client repo >>= fun t ->
  Store.head_exn (t "head") >>= fun head ->
  (* XXX: small race here *)
  View.of_path (t "client view") [] >>= fun client_view ->
  Store.master task repo >>= fun master ->
  View.of_path (t "master view") [client] >>= fun master_view ->
  View.diff client_view master_view >>= function
  | [] -> Lwt.return_unit
  | _  ->
    let msg =
      Printf.sprintf "Merge %s:%s" client (Irmin.Hash.SHA1.to_hum head)
    in
  (* XXX: small race again *)
    View.update_path (master msg) [client] client_view

type t = {
  mutable clients    : StringSet.t;
  mutable merge_hooks: (string * (unit -> unit Lwt.t)) list;
}

let empty () = { clients = StringSet.empty; merge_hooks = [] }

let watch t repo client =
  if client = "master" || StringSet.mem client t.clients then
    Lwt.return_unit
  else (
    show "Listening to a new client: %s." (Dog_misc.blue_s client);
    t.clients <- StringSet.add client t.clients;
    let merge () = update_subtree repo client in
    merge () >>= fun () ->
    t.merge_hooks <- (client, merge) :: t.merge_hooks;
    Lwt.return_unit
  )

let unwatch t client =
  if client = "master" || not (StringSet.mem client t.clients) then
    ()
  else (
    show "Stop listening to %s." (Dog_misc.blue_s client);
    t.clients <- StringSet.remove client t.clients;
    t.merge_hooks <- List.filter (fun (c,_) -> c <> client) t.merge_hooks;
  )

let watch_new t repo  =
  Store.Repo.branches repo >>= fun current_clients ->
  let current_clients = StringSet.of_list current_clients in
  let new_clients = StringSet.diff current_clients t.clients in
  let del_clients = StringSet.diff t.clients current_clients in
  StringSet.iter (unwatch t) del_clients;
  Lwt_list.iter_s (watch t repo) (StringSet.elements new_clients)

let cmd ~root =
  Store.Repo.create (config ~root) >>= fun repo ->
  Store.master task repo >>= fun t ->
  Store.Repo.branches repo >>= fun clients ->
  let clients = List.filter ((<>) "master") clients in
  (* FIXME: can have multiple clients? maybe packed vs. loose
     references *)
  let clients = StringSet.of_list clients |> StringSet.elements in
  let () = match clients with
    | []  -> ()
    | [c] -> show "Existing client: %s" @@ Dog_misc.blue_s c
    | _   -> show "Existing client(s): %s\n%!"
               (String.concat " " (List.map Dog_misc.blue_s clients))
  in
  let ts = Dog_misc.timestamp () in
  Store.update (t "Starting the server") [".started"] ts >>= fun () ->
  Irmin_unix.install_dir_polling_listener 1.;
  let state = empty () in
  let _unwatch =
    Store.Repo.watch_branches repo (fun _ _ ->
        watch_new state repo >>= fun () ->
        Lwt_list.iter_s (fun (_, m) -> m ()) state.merge_hooks
      ) in
  let t, _ = Lwt.task () in
  t
