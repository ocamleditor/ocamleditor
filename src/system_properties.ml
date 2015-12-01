(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

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


open Miscellanea
open Printf

(** get_locale *)
let get_locale () =
  try
    if Sys.win32 then begin
      let lines = Cmd.exec_lines "reg query \"hkcu\\Control Panel\\International\" /v LocaleName" in
      let lines = List.map String.trim lines in
      let locale =
        List.find (fun l -> Str.string_match (Str.regexp "LocaleName.+") l 0) lines
      in
      Str.string_match (Str.regexp ".*[\t ]\\([a-zA-Z-][a-zA-Z-][a-zA-Z-][a-zA-Z-][a-zA-Z-]\\)") locale 0 |> ignore;
      Some (Str.matched_group 1 locale)
    end else begin
      let lines = Cmd.exec_lines "locale" in
      let locale =
        List.find (fun l -> Str.string_match (Str.regexp ".*=.+") l 0) lines
      in
      Str.string_match (Str.regexp ".*=\\(.*\\)") locale 0 |> ignore;
      Some (Str.matched_group 1 locale)
    end
  with ex ->
    Printf.eprintf "File \"miscellanea.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
    None

(** findlib_package_exists *)
let findlib_package_exists packagename =
  let cmd = sprintf "ocamlfind query -format %%v %s %s" packagename Cmd.redirect_stderr in
  match Cmd.exec_lines cmd with
    | line :: _ when line = "" -> Some "OK"
    | line :: _ -> Some line
    | _ -> None

(** print *)
let to_string () =
  let buf = Buffer.create 1000 in
  let copyright = Str.replace_first (Str.regexp_string "©") "(c)" About.copyright in
  Printf.bprintf buf "\n%s %s\n\n%!" About.program_name About.version (*copyright*);
  let ocaml_version = Str.global_replace
    (Str.regexp "\n") " - " (Str.global_replace (Str.regexp "\n$") "" (Cmd.expand "ocamlc -v")) in
  let a, b, c = GMain.Main.version in
  Printf.bprintf buf "-------------------------------------------------------------------------------\n%!" ;
  let groups = [
    10, "ocaml";
    20, "ocamleditor";
    30, "msvc";
    40, "gtk";
    50, "package";
    60, "system";
    70, "command";
  ] in
  let properties = [
    10, "version", ocaml_version;
    20, "user_home", App_config.ocamleditor_user_home;
    20, "executable", Sys.executable_name;
    20, "icons", App_config.application_icons;
    20, "plugins", App_config.application_plugins;
    20, "oebuild", Oe_config.oebuild_command;
    20, "native_compilation", (match Ocaml_config.can_compile_native () with Some x -> "Yes (" ^ x ^ ")" | _ -> "No");
    20, "OCAMLEDITOR_MINGW", (try Sys.getenv "OCAMLEDITOR_MINGW" with Not_found -> "<Not Found>");
  ] @
  (if Sys.win32 && not App_config.is_mingw then [
     20, "oeproc", Oe_config.oeproc_command;
     30, "cl", (Opt.default Oe_config.cl "<Not Found>");
     30, "ml", (Opt.default Oe_config.ml "<Not Found>");
     30, "VSINSTALLDIR", (try Sys.getenv "VSINSTALLDIR" with Not_found -> "<Not Found>");
     30, "rc", (Opt.default Oe_config.rc "<Not Found>");
     30, "cvtres", (Opt.default Oe_config.cvtres "<Not Found>");
   ] else [
     70, "xdg-open", (Opt.default Oe_config.xdg_open_version "<Not Found>");
   ]) @ [
    70, "dot", (Opt.default Oe_config.dot_version "<Not Found>");
    70, "ocp-indent", (Opt.default Oe_config.ocp_indent_version "<Not Found>");
    50, "findlib", (Opt.default (findlib_package_exists "findlib") "<Not Found>");
    50, "lablgtk2", (Opt.default (findlib_package_exists "lablgtk2") "<Not Found>");
    50, "diff", (Opt.default (findlib_package_exists "diff") "<Not Found>");
    50, "lablgtk2.rsvg", (Opt.default (findlib_package_exists "lablgtk2.rsvg") "<Not Found>");
    50, "curl", (Opt.default (findlib_package_exists "curl") "<Not Found>");
    70, "git", (Opt.default Oe_config.git_version "<Not Found>");
    40, "version", (sprintf "%d.%d.%d" a b c);
    60, "locale", (Opt.default (get_locale ()) "<Not Found>");
    60, "charset", (let x, charset = Glib.Convert.get_charset () in sprintf "%b, %s" x charset);
    20, "backtrace_status", (sprintf "%b" (Printexc.backtrace_status ()));
  ] in
  let properties = List.sort compare properties in
  let properties = List.map (fun (g, n, v) -> (List.assoc g groups) ^ "." ^ n, v) properties in
  List.iter (bprintf buf "%s\n") (Text_util.dot_leaders properties);
  Printf.bprintf buf "-------------------------------------------------------------------------------\n%!" ;
  Buffer.contents buf
