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
  check `Set;
  check `Append;
  check `Json;
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

let merges: Dog.merges = [
  Dog.pattern_exn "f?o/**", `Replace;
  Dog.pattern_exn "foo"   , `Set;
]

let simple_merges () =
  let check x y = assert_bool (String.concat "/" x) (Dog.merge merges x = y) in
  check ["foo"] `Set;
  check ["foo"; ""] `Replace;
  check ["fo"]  `Replace

let simple_dot_merges () =
  let str = Dog.string_of_merges merges in
  eprintf "merges:\n%s\n%!" str;
  let merges' = Dog.merges_of_string str in
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
  "merges" , `Quick, simple_merges;
  ".merges", `Quick, simple_dot_merges;
]

let () =
  Alcotest.run "dog" [
    "simple", simple
  ]
