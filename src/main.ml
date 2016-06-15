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

open Cmdliner
open Printf

(* Help sections common to all commands *)
let copts_sect = "COMMON OPTIONS"
let help_secs = [
  `S copts_sect;
  `P "These options are common to all commands.";

  `S "AUTHORS";
  `P "Thomas Gazagnaire   <thomas@gazagnaire.org>";

  `S "BUGS";
  `P "Check bug reports at https://github.com/samoht/dog/issues.";
]

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let term_info title ~doc ~man =
  let man = man @ help_secs in
  Term.info ~sdocs:copts_sect ~doc ~man title

(* Converters *)

let pr_str = Format.pp_print_string

let run t =
  Lwt_unix.run (
    Lwt.catch
      (fun () -> t)
      (function e -> eprintf "%s\n%!" (Printexc.to_string e); exit 1)
  )

let mk (fn:'a): 'a Term.t =
  Term.(pure (fun () -> fn) $ setup_log)

let cwd = Sys.getcwd ()

let root =
  let doc =
    Arg.info ~docv:"ROOT" ~doc:"The repository root." ["r";"root"] in
  Arg.(value & opt string cwd & doc)

let client_name =
  let doc = Arg.info ~docv:"NAME" ~doc:"The client name." [] in
  Arg.(required & pos 0 (some string) None & doc)

let remote =
  let doc = Arg.info ~docv:"URI" ~doc:"The Git server URI." [] in
  Arg.(required & pos 1 (some string) None & doc)

let interval =
  let doc =
    Arg.info ~docv:"SECS" ~doc:"Seconds to wait between updates."
      ["n";"interval"]
  in
  Arg.(value & opt (some float) None & doc)

let once =
  let doc = Arg.info ~doc:"Watch for changes only once and exit." ["once"]in
  Arg.(value &flag doc)

(* WATCH *)
let watch_doc = "Watch a directory."
let watch_cmd =
  let man = [] in
  let watch once interval root name remote =
    run (Dog_watch.cmd ~once ?interval ~root name remote)
  in
  Term.(mk watch $ once $ interval $ root $ client_name $ remote),
  term_info "watch" ~doc:watch_doc ~man

(* LISTEN *)
let listen_doc = "Listen for incoming client connections"
let listen_cmd =
  let man = [
    `P "Listen for pushes on client branches and merge then back into master.";
  ] in
  let listen root = run (Dog_listen.cmd ~root) in
  Term.(mk listen $ root),
  term_info "listen" ~doc:listen_doc ~man

(* HELP *)
let help_cmd =
  let doc = "Display help about Dog and Dog commands." in
  let man = [
    `P "Use `$(mname) help topics' to get the full list of help topics.";
  ] in
  let topic =
    let doc = Arg.info [] ~docv:"TOPIC" ~doc:"The topic to get help on." in
    Arg.(value & pos 0 (some string) None & doc )
  in
  let help man_format cmds topic = match topic with
    | None       -> `Help (`Pager, None)
    | Some topic ->
      let topics = "topics" :: cmds in
      let conv, _ = Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
      match conv topic with
      | `Error e                -> `Error (false, e)
      | `Ok t when t = "topics" -> List.iter print_endline cmds; `Ok ()
      | `Ok t                   -> `Help (man_format, Some t) in
  Term.(ret (mk help $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "Dog, A loyal and faithful synchronisation tool." in
  let man = [
    `S "DESCRIPTION";
    `P "FIXME";
  ] in
  let usage () =
    printf
      "usage: dog [--version]\n\
      \           [--help]\n\
      \           <command> [<args>]\n\
      \n\
      The most commonly used subcommands are:\n\
      \    watch       %s\n\
      \    listen      %s\n\
      \n\
      See `dog help <command>` for more information on a specific command.\n%!"
      watch_doc listen_doc
  in
  Term.(pure usage $ setup_log),
  Term.info "dog"
    ~version:"%%VERSION%%"
    ~sdocs:copts_sect
    ~doc
    ~man

let cmds = [
    help_cmd;
    watch_cmd;
    listen_cmd;
  ]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
