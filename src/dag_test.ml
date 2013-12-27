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

(*

#load "C:\\ocaml\\lib\\str.cma";;
#load "C:\\ocaml\\lib\\unix.cma";;
#directory "C:\\ocaml\\lib\\threads";;
#load "C:\\ocaml\\lib\\threads\\threads.cma";;
#use "topfind";;
#thread;;
#require "threads";;
#require "threads.posix";;
#directory "C:\\ocaml\\devel\\ocamleditor\\src\\oebuild";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\common.cma";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\oebuildlib.cma";;

*)

open Printf

type parfold_entry = {
  pf_cmd         : string;
  pf_out         : Buffer.t;
  pf_err         : Buffer.t;
  pf_process_in  : (in_channel -> unit);
  pf_process_err : (in_channel -> unit);
}

let _ = Oebuild_util.crono ~label:"Sys.command" Sys.command "ocamldep.opt *.ml"

let _ =
  Oebuild_util.crono ~label:"parfold_command" (fun () -> Oebuild_dep.parfold_command ~command:"ocamldep.opt" ~verbose:false ~args:[
      "plugin.ml";
      "oe_config.ml";
      "gtkThread2.ml";
      "text_util.ml";
      "text_options.ml";
      "smart_keys.ml";
      "alignment.ml";
      "ocp_indent.ml";
      "gutter.ml";
      "color.ml";
      "text_init.ml";
      "text_indent_lines.ml";
      "task.ml";
      "preferences.ml";
      "bconf_old_1.ml";
      "target.ml";
      "rconf.ml";
      "oe.ml";
      "editor_file_type.ml";
      "plugins.ml";
      "editor_file.ml";
      "build_script_command.ml";
      "build_script_args.ml";
      "build_script.ml";
      "prj.ml";
      "convert.ml";
      "bookmark.ml";
      "project.ml";
      "mark_occurrences.ml";
      "line_num_labl.ml";
      "comments.ml";
      "lexical.ml";
      "lex.ml";
      "delimiters.ml";
      "text.ml";
      "process.mli";
      "process.ml";
      "task_process.ml";
      "messages_tab.ml";
      "messages.ml";
      "timeout.ml";
      "dot_viewer_pdf.ml";
      "dot_viewer_plugin.ml";
      "menu_view.ml";
      "menu_types.ml";
      "file_history.ml";
      "menu_file.ml";
      "location_history.ml";
      "incremental_search.ml";
      "preferences_apply.ml";
      "shell.ml";
      "ocaml_word_bound.ml";
      "code_folding.ml";
      "ocaml_text.ml";
      "print_type.ml";
      "error_indication.ml";
      "lexical_markup.ml";
      "search_results.ml";
      "find_text.ml";
      "find_text_in_buffer.ml";
      "find_text_output.ml";
      "find_text_dialog.ml";
      "binannot.ml";
      "binannot_ident_scan.ml";
      "binannot_ident.ml";
      "menu_search.ml";
      "editor_menu.ml";
      "dot.ml";
      "binannot_type.ml";
      "cmt_view.ml";
      "autosave.ml";
      "err_parser.mli";
      "err_parser.ml";
      "err_lexer.ml";
      "error.ml";
      "autocomp.ml";
      "annot_type.ml";
      "editor_page.ml";
      "editor_dialog.ml";
      "editor.ml";
      "task_console.ml";
      "toolbar.ml";
      "templates.ml";
      "build_id.ml";
      "about.ml";
      "template.ml";
      "gtk_theme.ml";
      "gtk_util.ml";
      "templ.ml";
      "symbol.mli";
      "symbol.ml";
      "quick_file_chooser.ml";
      "project_xml.ml";
      "target_page_deps.ml";
      "findlib_list.ml";
      "target_page.ml";
      "target_list.ml";
      "entry_list.ml";
      "entry_list_env.ml";
      "entry_list_args.ml";
      "args_env_widget.ml";
      "rconf_page.ml";
      "rconf_list.ml";
      "ocaml_home.ml";
      "etask_page.ml";
      "project_properties.ml";
      "ocaml_shell.ml";
      "prf.ml";
      "print_debug_info.ml";
      "preferences_tool.ml";
      "build_script_trg_widget.ml";
      "oebuild_script.ml";
      "build_script_printer.ml";
      "build_script_cmds_widget.ml";
      "build_script_args_widget.ml";
      "build_script_ui.ml";
      "menu_project.ml";
      "menu_help.ml";
      "oe_doc.ml";
      "mbrowser_slist.ml";
      "mbrowser_tool.ml";
      "mbrowser_compl.ml";
      "external_tools.ml";
      "menu.ml";
      "dialog_find_file.ml";
      "annotation.ml";
      "browser.ml";
      "ocamleditor_lib.ml";
      "ocamleditor.ml";
    ] () |> ignore; printf "\n%!") ();;


(*(** Test *)
let _ =
  let path = "C:\\ocaml\\devel\\ocamleditor\\src" in
  let command = "\
ocamlc.opt  \
-c -g -bin-annot -thread \
-I C:/ocaml/lib/compiler-libs \
-I C:\\ocaml\\lib\\site-lib\\lablgtk2 \
-I C:\\ocaml\\lib\\site-lib\\xml-light \
-I +ocamldoc -I gmisclib -I common -I icons -I otherwidgets -I oebuild \
" in
  kprintf Sys.command "rm %s\\*.cmi 2>NUL" path |> ignore;
  kprintf Sys.command "rm %s\\*.cmo 2>NUL" path |> ignore;
  kprintf Sys.command "rm %s\\*.cmx 2>NUL" path |> ignore;
  kprintf Sys.command "rm %s\\*.cmt 2>NUL" path |> ignore;
  kprintf Sys.command "rm %s\\*.obj 2>NUL" path |> ignore;
  let cb_create_command filename = Some (sprintf "%s %s" command filename) in
  let cb_at_exit _ = () in
  let dag = crono ~label:"create_dag" (fun () ->
      Oebuild_parallel.create_dag
        ~cb_create_command
        ~cb_at_exit
        ~toplevel_modules:["ocamleditor.ml"]) () in
(*  let dag = crono ~label:"create_dag" (fun () -> create_dag ~command ~toplevel_modules:["oebuild/oebuild_tool.ml"]) () in*)
  (*let compile_time = Oebuild_parallel.process_parallel dag in*)
  (*Printf.printf "Compile time: %f\n%!" compile_time;*)()
;;*)




