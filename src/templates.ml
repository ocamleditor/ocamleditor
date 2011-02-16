(*

  OCamlEditor
  Copyright (C) 2010, 2011 Francesco Tovagliari

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

type t =
  | Templ of templ
  | Action of (Ocaml_text.view -> unit)

and templ = t_elem list

and t_elem =
  | SELECTION        (* Inserts the currently-selected text *)
  | SELECTION_TRIM   (* Inserts the currently-selected text without trailing and leading whitespace *)
  | SELECTION_OPT of string (* Inserts the currently-selected text; if none inserts the text specified *)
  | IN               (* Increases the current indentation level *)
  | OUT              (* Decreases the current indentation level *)
  | T0 of string     (* Inserts a string with the same indentation as the line preceding the template *)
  | TI of string     (* Inserts a string with the current indentation level *)
  | T of string      (* Inserts a string without indenting *)
  | NL               (* Inserts a newline *)
  | CURRENT_LINE
  | CURRENT_FILENAME (* Inserts the name of the currently open file *)
  | DESCRIPTION      (* Project description *)
  | I                (* Represents the insertion point *)
  | S                (* Represents the selection bound *)



(** spec
    template_name * description * code
*)
let spec : (string * string * t) list =
  (** Actions *)
  let actions : ((string * string * (Ocaml_text.view -> unit)) list) = [
    "() remove", "Remove delimiters", begin fun view ->
      let view = (view :> Text.view) in
      let where = view#buffer#get_iter `INSERT in
      view#matching_delim_goto ~select:true ();
      let start, stop = view#buffer#selection_bounds in
      let text = Miscellanea.trim (view#buffer#get_text ~start ~stop ()) in
      view#buffer#place_cursor ~where;
      view#matching_delim_goto ~select:true ~strict:false ();
      let start, stop = view#buffer#selection_bounds in
      view#matching_delim_remove_tag ();
      view#buffer#delete ~start ~stop;
      view#buffer#insert text;
      view#matching_delim_remove_tag ();
      let iter = view#buffer#get_iter `INSERT in
      view#buffer#select_range iter (iter#backward_chars (Glib.Utf8.length text));
    end;
    "align", "Align/collapse definitions", (fun view -> view#align_definitions());
  ] in
  (** Templates *)
  let templates = [
    "()", "(<selection>)", [T "("; SELECTION_TRIM ; T ")"; I];

    "descr", "Project description", [DESCRIPTION];

    "be", "begin <selection> end",
      [T0 "begin"; NL; IN; SELECTION; OUT; T0 "end;"; I; NL];

    "ifthen", "if ... then begin <selection> end else begin ... end;", [
      T0 "if "; I; T " then begin"; NL;
      IN; SELECTION; OUT;
      T0 "end else begin"; NL;
      TI "end;"; NL;
    ];

    "ifte", "if <selection> then begin ... end else begin ... end", [
      T "if "; I; SELECTION; S; T " then begin"; NL;
      IN; TI ""; NL; OUT;
      TI "end else begin"; NL;
      IN; TI ""; NL; OUT;
      TI "end;";
    ];

    "ign", "ignore (<selection>)", [I; T "ignore ("; SELECTION; T ")"; I];

    "ignbe", "ignore begin ... end",
      [T0 "ignore begin"; NL; IN; SELECTION; OUT; T0 "end;"; I; NL];

    "letfun", "let <selection> = function ...", [
      T "let "; SELECTION_TRIM; T " = function "; NL;
      IN; TI "| "; I; NL;
      TI "| _ -> "
    ];

    "letfunin", "let <selection> = function ... in", [
      T "let "; SELECTION_TRIM; T " = function "; NL;
      IN; TI "| "; I; T " -> "; NL;
      TI "| _ -> "; NL;
      OUT; TI "in";
    ];

    "errback", "Print backtrace on standard error", [
      T "Printf.eprintf \"File \\\""; CURRENT_FILENAME; T"\\\": %s\\n%s\\n%!\" (Printexc.to_string "; I;
      SELECTION_OPT "ex"; S; T ") (Printexc.get_backtrace());"
    ];

    "errassert", "if not (<selection>) then begin eprintf ...; assert false end;", [
      T "if not ("; SELECTION_TRIM; T ") then begin"; NL;
      IN; TI "Printf.eprintf \""; I; T"Assertion failed: "; SELECTION_TRIM; T""; S; T"\\n%!\";"; NL;
      TI "assert false;"; OUT; NL;
      TI "end;";
    ];

    "print", "Printf.printf \"<selection> = %s\\n%!\" <selection>;",
      [T "Printf.printf \""; SELECTION_TRIM; T " = %"; I; T "s"; S; T "\\n%!\" "; SELECTION_TRIM; T ";"];

    "matbe", "begin match ... with ... -> <selection> | _ -> ... end;", [
      T0 "begin"; NL;
      IN; TI "match "; I; T" with"; NL;
      IN; TI "| "; T" ->"; NL;
      IN; SELECTION;
      OUT; TI "| _ -> "; NL;
      OUT; OUT; T0 "end;"; NL
    ];

    "matsel", "match <selection> with ...", [
      T "match "; SELECTION_TRIM; T " with "; NL;
      IN; TI "| "; I; T" -> "; NL;
      TI "| _ -> "; NL
    ];

    "matsome", "match <selection> with | Some x -> ... | _ -> ...",
      [T "match "; SELECTION_TRIM; T " with "; NL; IN; TI "| Some x -> "; NL; TI "| _ -> "; I; NL];

    "matsomeinline", "match <selection> with Some x | _ -> ...",
      [T "match "; SELECTION_TRIM; T " with Some x -> "; I; T " | _ -> "];

    "try", "try <selection> with ...",
      [T0 "try"; NL; IN; SELECTION; OUT; T0 "with "; I; T " -> "; NL];

    "tryf", "let finally = ... in try <slection>; finally() with ex -> (finally())", [
      T0 "let finally () = "; I; T " in"; NL;
      T0 "try"; NL;
      IN; SELECTION; TI "finally();"; NL;
      OUT; T0 "with ex -> (finally())"; NL
    ];

    "trybe", "begin try <selection> with _ -> ... end", [
      T0 "begin"; NL;
      IN; TI "try"; NL;
      IN; SELECTION;
      OUT; TI "with "; I; T " -> "; NL;
      OUT; T0 "end;"; NL
    ];

    "callback", "~callback:begin fun () -> ... end",
      [T " ~callback:begin fun () ->"; IN; NL; TI ""; I; NL; OUT; TI "end;"];

    "for", "for ... do ... done", [
      TI "for i = 0 to "; I; T " do"; NL;
      IN; SELECTION; OUT;
      T0 "done;"; NL;
    ];

    "while", "while ... do ... done", [
      TI "while "; I; T " do"; NL;
      IN; SELECTION; OUT;
      T0 "done;"; NL;
    ];

    "q", "(* <selection> *)", [T "(*"; SELECTION; T "*)"];
    "qb", "(* <block> *)", [T0 "(*"; NL; SELECTION; T0 "*)"; NL];
    "qbi", "(* <block> *)", [T0 "(*"; NL; IN; SELECTION; OUT; T0 "*)"; NL];

    "qq", "(** <selection> *)", [T "(**"; SELECTION; T "*)"];
    "qqb", "(** <block> *)", [T0 "(**"; NL; SELECTION; T0 "*)"; NL];
    "qqbi", "(** <block> *)", [T0 "(**"; NL; IN; SELECTION; OUT; T0 "*)"; NL];
  ] in
  let actions = List.map (fun (a, b, c) -> a, b, Action c) actions in
  let templates = List.map (fun (a, b, c) -> a, b, Templ c) templates in
  List.sort (fun (x1, _, _) (x2, _, _) -> Pervasives.compare x1 x2) (actions @ templates)






