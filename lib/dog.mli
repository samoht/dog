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

open Dog_misc

(** {1 Merge Strategies} *)

type merge =
  [ `Ignore
  | `Replace
  | `Set
  | `Append
  | `Json ]
(** The different merge strategies.

    {ul
    {- [Ignore] skips the client file.}
    {- [Replace] replaces the server file by the client one.}
    {- [Set] adds the client lines to the server files.}
    {- [Append] appends lines at the end of the server files.}
    {- [Json] considers the file as a JSON value and merge the value on
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

type merges = (pattern * merge) list
(** FIXME *)

val merge: merges -> path -> merge
(** FIXME *)

val dot_merge_file: path
(** [dot_merge] is [[".merge"]]. *)

val default_merges: (pattern * merge) list
(** The default merge pattern is the rule [dot_merge -> `Set]. *)

val string_of_merges: merges -> string
(** FIXME *)

val merges_of_string: string -> merges
(** FIXME *)


(** {1 Files} *)

type file
(** The type for files. *)

(** {1 Commands} *)

type t = ([`BC], path, file) Irmin.t
(** The type for Dog stores. *)

type 'a callback = (unit -> merges Lwt.t) -> (string -> t) -> string -> 'a Lwt.t
(** The type for store callbacks. A callback is a function taking the
    store merges' parameters, a store handler and the store's current
    tag as parameters. *)

val with_store: root:string -> 'a callback -> 'a Lwt.t
(** [with_store ~root f] loads the configuration stored in
    {!dot_merge_file} and apply the function [f] to the resulting
    store.  *)

val listen: root:string -> unit Lwt.t
(** FIXME *)
