/*

  OCamlEditor
  Copyright (C) 2010 Francesco Tovagliari

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

*/

%{
open Printf
open Oe
let re_inconsistent_assumptions = Str.regexp
  ".*make[ \t\r\n]+inconsistent[ \t\r\n]+assumptions[ \t\r\n]+over[ \t\r\n]+\\(interface\\|implementation\\)[ \t\r\n]+\\([^ \t\r\n]+\\)[ \t\r\n]*"
%}

%token <string * string * int * int * int *int> LOCATION
%token <string> LINE_OF_MESSAGE
%token <string> ALERT
%token EOF ERROR
%token <int> WARNING

%start compiler_output
%type <Oe.error_message list> compiler_output
%%
compiler_output:
  | /* empty */                           { [] }
  | message compiler_output               { $1 :: $2 }
;
message:
  | LOCATION level line_of_message {
    let message_text = String.concat "\n" $3 in
    let inconsistent_assumptions =
      match $2 with
        | Error ->
          let is_inconsistent_assumptions = Str.string_match re_inconsistent_assumptions message_text 0 in
          if is_inconsistent_assumptions then begin
            let modname = Str.matched_group 2 message_text in
            Some modname
          end else None
        | _ -> None
    in
    let loc, filename, line0, line1, c1, c2 = $1 in {
      er_filename = filename;
      er_lines = (line0, line1);
      er_characters = (c1, c2);
      er_level = $2;
      er_location = loc;
      er_message = (
      (
        match $2 with 
        | Error -> "Error: "
        | Warning x -> sprintf "Warning %d: " x
        | Alert s -> "Alert " ^ s ^ ": "
      ) ^ message_text);
      er_inconsistent_assumptions = inconsistent_assumptions;
    }}
  | LOCATION line_of_message {
    let message_text = String.concat "\n" $2 in
    let loc, filename, line0, line1, c1, c2 = $1 in {
      er_filename = filename;
      er_lines = (line0, line1);
      er_characters = (c1, c2);
      er_level = Error;
      er_location = loc;
      er_message = message_text;
      er_inconsistent_assumptions = None;
      }
  }
;
line_of_message:
  | /* empty */                     { [] }
  | LINE_OF_MESSAGE line_of_message { $1 :: $2}
;
level:
  | WARNING { Warning $1 }
  | ALERT { Alert $1 }
  | ERROR   { Error }
;





