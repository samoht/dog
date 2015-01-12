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

(** The Dog API. *)

(** {1 Merge Strategies} *)

type merge =
  [ `Ignore
  | `Replace
  | `Line_set
  | `Line_append
  | `Jsonx ]
(** The different merge strategies.

    {ul
    {- [Ignore] skips the client file.}
    {- [Replace] replaces the server file by the client one.}
    {- [Line_set] adds the client lines to the server files.}
    {- [Line_append] appends lines at the end of the server files.}
    {- [Jsonx] considers the file as a JSON value and merge the value on
    the server with the client version. The `x` stands for the special
    merge semantics that we use: records are considered as k/v maps and
    are merged as follow: if the key exists on both the client and the
    server, then merge the values together -- otherwise always add new
    client keys. Arrays are considered as unordered sets. FIXME}
    }
*)

val string_of_merge: merge -> string
(** FIXME *)

val merge_of_string: string -> merge option
(** FIXME *)

val merge_of_string_exn: string -> merge
(** FIXME *)

(** {1 Configurations} *)

type pattern
(** The type for globs patterns. *)

val pattern: string -> [`Ok of pattern | `Error of string]
(** Create a pattern. *)

val pattern_exn: string -> pattern
(** Same as {!pattern} but raise [Failure] in case of error. *)

val compare_pattern: pattern -> pattern -> int
(** Pattern comparator. *)

val string_of_pattern: pattern -> string
(** FIXME *)

val check: pattern -> (string -> bool)
(** Check a pattern. *)

type conf
(** The type for client configurations. *)

val conf: client:string -> server:Uri.t -> merges:(pattern * merge) list
  -> conf
(** Build a configuration value for a Dog client's store. *)

val client: conf -> string
(** The client's name, used as commit's username. *)

val server: conf -> Uri.t
(** The root of the filesystem managed by the client. *)

val merges: conf -> (pattern * merge) list
(** FIXME *)

val merge: conf -> string -> merge
(** [merge conf file] is the merge strategy associated with the
    filename [file]. *)

val toml_of_conf: conf -> Toml.Value.table
(** FIXME *)

val conf_of_toml: Toml.Value.table -> conf
(** FIXME *)

(** {1 Files} *)

type file
(** The type for files. *)

val conf_path: string list
(** [conf_path] is [[".dog"]]. *)

(** {1 Commands} *)

type t = (string list, file) Irmin.t
(** The type for Dog stores. *)

val with_store: root:string -> string -> (t -> 'a Lwt.t) -> 'a Lwt.t
(** [with_store ~root msg f] loads the configuration stored in
    {!conf_path} and apply the function [f] to the resulting
    store. Use [msg] as commit message if necessary. *)

val init: root:string -> conf -> unit Lwt.t
(** FIXME *)

val list_merges: root:string -> (pattern * merge) list Lwt.t
(** FIXME *)

val add_merge: root:string -> pattern -> merge -> unit Lwt.t
(** FIXME *)

val remove_merge: root:string -> pattern -> unit Lwt.t
(** FIXME *)

val push: root:string -> string -> unit Lwt.t
(** FIXME *)

val listen: root:string -> unit Lwt.t
(** FIXME *)
