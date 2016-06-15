#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"

open Topkg

let opam = Pkg.opam_file ~lint_deps_excluding:(Some ["irmin-unix"]) "opam"

let () =
  Pkg.describe ~metas:[] ~opams:[opam] "dog" @@ fun c ->
  Ok [ Pkg.bin "src/main" ~dst:"dog" ]
