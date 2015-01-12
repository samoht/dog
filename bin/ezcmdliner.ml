(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2014 Louis Gesbert
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

let mk_subdoc ?(defaults=[]) commands =
  let bold s = Printf.sprintf "$(b,%s)" s in
  let it s = Printf.sprintf "$(i,%s)" s in
  `S "COMMANDS" ::
  (List.map (function
       | "", name ->
         `P (Printf.sprintf "Without argument, defaults to %s."
               (bold name))
       | arg, default ->
         `I (it arg, Printf.sprintf "With a %s argument, defaults to %s %s."
               (it arg) (bold default) (it arg))
     ) defaults) @
  List.map (fun (cs,_,args,d) ->
      let cmds = String.concat ", " (List.map bold cs) ^ " " ^
                 String.concat " " (List.map it args) in
      `I (cmds, d)
    ) commands @
  [`S "OPTIONS"] (* Ensures options get after commands *)

let mk_subcommands_aux my_enum commands =
  let command =
    let doc = Arg.info ~docv:"COMMAND" [] in
    let commands =
      List.fold_left
        (fun acc (cs,f,_,_) -> List.map (fun c -> c,f) cs @ acc)
        [] commands in
    Arg.(value & pos 0 (some & my_enum commands) None & doc) in
  let params =
    let doc = Arg.info ~doc:"Optional parameters." [] in
    Arg.(value & pos_right 0 string [] & doc) in
  command, params

let mk_subcommands commands =
  mk_subcommands_aux Arg.enum commands

let mk_subcommands_with_default commands =
  let enum_with_default sl: 'a Arg.converter =
    let parse, print = Arg.enum sl in
    let parse s =
      match parse s with
      | `Ok _ as x -> x
      | _ -> `Ok (`default s) in
    parse, print
  in
  mk_subcommands_aux enum_with_default commands

let str = Printf.sprintf

let pretty_list t = match List.rev t with
  | []   -> "";
  | h::t -> str "%s and %s" (String.concat ", " (List.rev t)) h

let bad_subcommand command subcommands usersubcommand userparams =
  match usersubcommand with
  | None ->
    `Error (false, Printf.sprintf "Missing subcommand. Valid subcommands are %s."
              (pretty_list
                 (List.flatten (List.map (fun (a,_,_,_) -> a) subcommands))))
  | Some (`default cmd) ->
    `Error (true, Printf.sprintf "Invalid %s subcommand %S" command cmd)
  | Some usersubcommand ->
    let exe = Filename.basename Sys.executable_name in
    match List.find_all (fun (_,cmd,_,_) -> cmd = usersubcommand) subcommands with
    | [name::_, _, args, _doc] ->
      let usage =
        Printf.sprintf "%s %s [OPTION]... %s %s"
          exe command name (String.concat " " args) in
      if List.length userparams < List.length args then
        `Error (false, Printf.sprintf "%s: Missing argument.\nUsage: %s\n"
                  exe usage)
      else
        `Error (false, Printf.sprintf "%s: Too many arguments.\nUsage: %s\n"
                  exe usage)
    | _ ->
      `Error (true, Printf.sprintf "Invalid %s subcommand" command)

let run default commands =
  Sys.catch_break true;
  let () =
    try Sys.set_signal Sys.sigpipe (Sys.Signal_handle (fun _ -> ()))
    with Invalid_argument _ -> ()
  in
  match Term.eval_choice default commands with
  | `Error _ -> exit 1
  | _        -> exit 0
