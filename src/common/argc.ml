(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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


module type COMMAND = sig
  type t
  val string_of_command : t -> string
  val command_of_string : string -> t
end;;

module Make (C : COMMAND) = struct
  open Printf

  type command_descr = string
  type speclist = (Arg.key * Arg.spec * Arg.doc) list

  exception Command_found

  let command : C.t option ref = ref None;;

  let rec split3 = function
      [] -> ([], [], [])
    | (x,y,z)::l ->
        let (rx, ry, rz) = split3 l in (x::rx, y::ry, z::rz);;

  let rpad txt c width =
    let result = txt ^ (String.make width c) in
    String.sub result 0 width;;

    let help_of_command cmd_map maxlength cmd =
      let spec , descr = List.assoc cmd cmd_map in
      (sprintf "  %s  %s" (rpad (C.string_of_command cmd) ' ' maxlength) descr)

    let help_of_commands commands cmd_map =
      let maxlength = List.fold_left (fun cand x -> max cand (String.length (C.string_of_command x))) 0 commands in
      (String.concat "\n" (List.map (help_of_command cmd_map maxlength) commands))

  let create_help_msg commands cmd_map global_speclist usage_msg =
    sprintf "%s\n\nGLOBAL OPTIONS%s\nCOMMANDS\n%s\n"
      usage_msg
      (Arg.usage_string global_speclist "")
      (help_of_commands commands cmd_map);;

  let with_command f = match !command with Some cmd -> f cmd | _ -> assert false;;

  let parse_argv
      args
      ~(global_options : speclist)
      ~(command_options : (C.t  * speclist * command_descr) list)
      ~anon_fun
      ?(usage_msg="USAGE\n  test [global_options*] <command> [args*] [options*]\n  test <command> --help")
      execute_command =
    command := None;
    Arg.current := 0;
    let commands, speclist, cmd_descr = split3 command_options in
    let cmd_map = List.map2 (fun c d -> (c, d)) commands (List.map2 (fun s d -> (s, d)) speclist cmd_descr) in
    let parse_anon arg =
      match !command with
        | None ->
          command := Some (C.command_of_string arg);
          raise Command_found
        | _ -> assert false
    in
    let help_string () =
      match !command with
        | Some cmd ->
          let spec, descr = List.assoc cmd cmd_map in
          let cmd = C.string_of_command cmd in
          Arg.usage_string spec
            (sprintf "%s %s - %s\n\nUSAGE\n  %s [global_options*] %s [args*] [options*]\n\nOPTIONS"
              args.(0) cmd descr args.(0) cmd)
        | _ -> create_help_msg commands cmd_map global_options usage_msg
    in
    if Array.length args = 1 then (raise (Arg.Help (help_string())));
    try Arg.parse_argv args global_options parse_anon usage_msg;
    with
      | Command_found ->
        let len = Array.length args - !Arg.current in
        let command_args = Array.create len "" in
        Array.blit args !Arg.current command_args 0 len;
        let parse_anon arg = with_command (fun cmd -> anon_fun cmd arg) in
        begin
          try
            Arg.current := 0;
            let speclist = with_command (fun cmd -> fst (List.assoc cmd cmd_map)) in
            Arg.parse_argv command_args speclist parse_anon usage_msg;
            let f = function
              | Some cmd -> execute_command cmd
              | None -> raise (Arg.Help (create_help_msg commands cmd_map global_options usage_msg))
            in
            f !command
          with
            | Arg.Help _ -> raise (Arg.Help (help_string ()))
            | Arg.Bad msg ->
              with_command (fun cmd ->
                let first_line = String.sub msg 0 (String.index msg '\n') in
                raise (Arg.Bad (sprintf "%s\n%s"
                  (*(C.string_of_command cmd) (command_args.(!Arg.current))*) first_line (help_string()))))
        end;
      | Arg.Bad _ ->
        raise (Arg.Bad (sprintf "unknown global option `%s'\n%s" (args.(!Arg.current)) (help_string())))
      | Arg.Help _ -> raise (Arg.Help (help_string()));;

  let parse ~global_options ~command_options ~anon_fun ?usage_msg f =
    parse_argv Sys.argv ~global_options ~command_options ~anon_fun ?usage_msg f;;
end;;

