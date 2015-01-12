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

open Alcotest
open OUnit
open Printf

let () =
  Log.set_log_level Log.DEBUG

let simple_merge () =
  let check x =
    let str = Dog.string_of_merge x in
    assert_bool str (Dog.merge_of_string str = Some x)
  in
  let neg_check str =
    assert_bool str (Dog.merge_of_string str = None)
  in
  check `Ignore;
  check `Replace;
  check `Line_set;
  check `Jsonx;
  check `Line_append;
  neg_check "foo";
  neg_check "bar"

let simple_pattern () =
  let check ?(neg=false) x y =
    match Dog.pattern x with
    | `Error _ -> assert_bool x false
    | `Ok pat  ->
      if neg then assert_bool x (not (Dog.check pat y))
      else assert_bool x (Dog.check pat y)
  in
  check "foo*"   "foo";
  check "foo*"   "foo/bar";
  check "foo*"   "foo/bar/xx";
  check "f?o/**" "fio/uuu";
  check ~neg:true "f?o/**" "fio"

let conf =
  Dog.conf ~client:"client" ~server:(Uri.of_string "server") ~merges:[
    Dog.pattern_exn "f?o/**", `Replace;
    Dog.pattern_exn "foo"   , `Line_set;
  ]

let simple_conf () =
  let check x y = assert_bool x (Dog.merge conf x = y) in
  check "foo"  `Line_set;
  check "foo/" `Replace;
  check "fo"   `Ignore

let simple_toml () =
  let toml = Dog.toml_of_conf conf in
  Toml.Printer.table Format.err_formatter toml;
  let conf' = Dog.conf_of_toml toml in
  assert_bool "server" (Dog.server conf = Dog.server conf');
  assert_bool "client" (Dog.client conf = Dog.client conf');
  let cmp (x,_) (y,_) = Dog.compare_pattern x y in
  let merges = List.sort cmp (Dog.merges conf) in
  let merges' = List.sort cmp (Dog.merges conf') in
  try
    let c = ref 0 in
    List.iter2 (fun (p1, m1) (p2, m2) ->
        incr c;
        assert_bool (sprintf "merge%d-pat" !c) (Dog.compare_pattern p1 p2 = 0);
        assert_bool (sprintf "merge%d-mer" !c) (m1 = m2);
      ) merges merges'
  with Not_found ->
    assert_bool "merge" false

let simple = [
  "merge"  , `Quick, simple_merge;
  "pattern", `Quick, simple_pattern;
  "conf"   , `Quick, simple_conf;
  "toml"   , `Quick, simple_toml;
]

let () =
  Alcotest.run "dog" [
    "simple", simple
  ]
