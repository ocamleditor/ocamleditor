(*

  OCamlEditor
  Copyright (C) 2010-2013 Francesco Tovagliari

  This file is part of OCamlEditor.

  OCamlEditor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  OCamlEditor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)


type command_descr = string
type command_usage = string
type speclist = (Arg.key * Arg.spec * Arg.doc) list

module type COMMAND = sig
  type t
  val string_of_command : t -> string
  val command_of_string : string -> t
  val options : (t * speclist * command_descr * command_usage) list
  val anon_fun : t -> string -> unit
end

module Make (C : COMMAND) = struct
  open Printf

  exception Command_found
  exception Help_Command of C.t * (speclist * command_descr * command_usage) * string

  let command : C.t option ref = ref None;;

  let commands, cmd_map =
    List.fold_left (fun (x, y) (a, b, c, d) -> a::x, (a, (b, c, d))::y) ([], []) C.options

  let rpad txt c width =
    let result = txt ^ (String.make width c) in
    String.sub result 0 width;;

  let help_of_commands =
    let help_of_command maxlength cmd =
      let spec , descr, _ = List.assoc cmd cmd_map in
      (sprintf "  %s  %s" (rpad (C.string_of_command cmd) ' ' maxlength) descr)
    in
    let maxlength = List.fold_left (fun cand x -> max cand (String.length (C.string_of_command x))) 0 commands in
    "\n" ^ (String.concat "\n" (List.map (help_of_command maxlength) (List.rev commands)))

  let create_help_msg global_speclist usage_msg =
    sprintf "%s\n\nGLOBAL OPTIONS%s\nCOMMANDS\n%s\n"
      usage_msg
      (Arg.usage_string global_speclist "") help_of_commands;;

  let with_command f = match !command with Some cmd -> f cmd | _ -> assert false;;

  let parse_argv
      args
      ~(global_options : speclist)
      ?default_command
      ?(usage_msg=sprintf "\nUSAGE\n  %s [global_options*] <command> [options*] [args*]\n  %s <command> --help" args.(0) args.(0))
      execute_command =
    command := None;
    Arg.current := 0;
    let parse_anon arg =
      match !command with
        | None ->
          let cmd =
            try C.command_of_string arg
            with ex ->
              decr Arg.current;
              (match default_command with Some c -> c | _ -> raise ex)
          in
          command := Some cmd;
          raise Command_found
        | _ -> assert false
    in
    let help_string () =
      match !command with
        | Some cmd ->
          let spec, descr, _ = List.assoc cmd cmd_map in
          let cmd = C.string_of_command cmd in
          Arg.usage_string spec
            (sprintf "%s %s - %s\n\nUSAGE\n  %s [global_options*] %s [options*] [args*]\n\nOPTIONS"
              args.(0) cmd descr args.(0) cmd)
        | _ -> create_help_msg global_options usage_msg
    in
    if Array.length args = 1 then (raise (Arg.Help (help_string())));
    try Arg.parse_argv args global_options parse_anon usage_msg;
    with
      | Command_found ->
        let len = Array.length args - !Arg.current in
        let command_args = Array.create len "" in
        Array.blit args !Arg.current command_args 0 len;
        let parse_anon = with_command (fun cmd -> C.anon_fun cmd) in
        begin
          try
            Arg.current := 0;
            let speclist = with_command (fun cmd -> let sp, _, _ =
              try List.assoc cmd cmd_map with Not_found -> assert false in sp) in
            Arg.parse_argv command_args speclist parse_anon usage_msg;
            let f = function
              | Some cmd -> execute_command cmd
              | None -> raise (Arg.Help (create_help_msg global_options usage_msg))
            in
            f !command
          with
            | Arg.Help _ ->
              with_command begin fun cmd ->
                let cmd_specs, cmd_descr, cmd_usage = List.assoc cmd cmd_map in
                raise (Help_Command  (cmd, (cmd_specs, cmd_descr, cmd_usage), help_string ()))
              end
            | Arg.Bad msg as ex ->
              with_command begin fun cmd ->
                try
                  let first_line = String.sub msg 0 (String.index msg '\n') in
                  raise (Arg.Bad (sprintf "%s\n%s"
                    (*(C.string_of_command cmd) (command_args.(!Arg.current))*) first_line (help_string())))
                with Not_found -> raise ex
              end
        end;
      | Arg.Bad _ ->
        raise (Arg.Bad (sprintf "unknown global option `%s'\n%s" (args.(!Arg.current)) (help_string())))
      | Arg.Help _ -> raise (Arg.Help (help_string()));;

  let parse ~global_options ?default_command ?usage_msg f =
    parse_argv Sys.argv ~global_options ?default_command ?usage_msg f;;
end;;

