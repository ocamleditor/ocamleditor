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


open Printf

let table : (string * string * (string -> string)) list ref = ref []

let register ~key ~descr ~f = table := (key, descr, f) :: !table;;

let help = sprintf "\
Custom templates are a set of functions grouped into an OCaml library that
%s loads at startup. Each of these functions corresponds to a template
and is applied when the user invoke the template from the editor.
The type of a template function is <tt>string &#8594; string</tt>, where the argument is the
text currently selected in the editor and the result is the text that will replace
the selection.

To create a template write an OCaml module that implements such a function
and registers it with the main program. To register a function use
<tt><b>Template</b>.register</tt>, you can find it in the %s compiled
source tree. The signature of <tt><b>Template</b>.register</tt> is as follows:

<tt>  <b>val</b> register :
    <b>key:</b>string ->
    <b>descr:</b>string ->
    <b>f:</b>(string -> string) -> unit</tt>

Here is a simple example that surrounds the selection with double quotes.
File <tt>example.ml</tt>:

<tt>  <b>let</b> surround text = \"\\\"\" ^ text ^ \"\\\"\";;

  <b>let</b> _ = <b>Template</b>.register
    <b>~key:</b>\"example\"
    <b>~descr:</b>\"Description of example\"
    <b>~f:</b>surround;;</tt>

Compile with the command:

  <tt>ocamlc -a -o example.cma -I <i>~/ocamleditor</i>/src example.ml</tt>

where the <tt>-I</tt> option refers to the %s compiled source tree, so
replace it with the exact location that you have on your system.

Additionally, if you want your templates to be loaded only when you
open a specific project, you can copy the library in:

<tt>  %s</tt>

" About.program_name
  About.program_name
  About.program_name
  (Filename.concat "<i>&lt;your-project-root&gt;</i>" Oe_config.template_project_filename);;


