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


open Miscellanea
open Printf
open Bconf

let print_configs ochan bconfigs =
  let i = ref 0 in
  let print = fprintf ochan "  %s\n" in
  output_string ochan "let targets = [\n";
  List.iter begin fun bc ->
    incr i;
    kprintf print "(\x2A %d \x2A)" !i;
    kprintf print "%S, {" bc.name;
    kprintf print "  output_name          = %S;" bc.outname;
    kprintf print "  output_kind          = %s;" (string_of_outkind bc.outkind);
    kprintf print "  compilation_bytecode = %b;" bc.byt;
    kprintf print "  compilation_native   = %b;" bc.opt;
    kprintf print "  toplevel_modules     = %S;" bc.files;
    kprintf print "  search_path          = %S; (\x2A -I \x2A)" bc.includes;
    kprintf print "  required_libraries   = %S;" bc.libs;
    kprintf print "  compiler_flags       = %S;" bc.cflags;
    kprintf print "  linker_flags         = %S;" bc.lflags;
    kprintf print "  thread               = %b;" bc.thread;
    kprintf print "  vmthread             = %b;" bc.vmthread;
    kprintf print "  pp                   = %S;" bc.pp;
    kprintf print "  library_install_dir  = %S; (\x2A Relative to the Standard Library Directory \x2A)" bc.lib_install_path;
    kprintf print "  other_objects        = %S;" bc.other_objects;
    kprintf print "};";
  end bconfigs;
  output_string ochan "];;\n";;

let create ~project ~filename () =
  let ochan = open_out_bin filename in
  let finally () = close_out_noerr ochan in
  try
    output_string ochan "(\x2A\n   Please edit the \"Build Configurations\" section at the end\n   of this file to set the right options for your system.\n\x2A)\n\n";
    output_string ochan Oebuild_script.code;
    output_string ochan "open Arg\n";
    output_string ochan "\n\n";
    output_string ochan "(\x2A Build Configurations ==================================================== \x2A)\n\n";
    (*  *)
    print_configs ochan project.Project.build;
    output_string ochan "\n";
    (*  *)
    output_string ochan "let _ = main targets\n";
    (*  *)
    finally();
  with ex -> begin
    finally();
    raise ex
  end;;













