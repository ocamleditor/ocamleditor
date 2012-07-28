(*

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

*)

{
open Annot_parser
exception Eof
}

let lf = "\x0A"
let sp = '\x20'
let line_of_data = "\x0A\x20\x20"

rule token = parse
  | eof                                                     { EOF }
  | lf                                                      { LF }
  | sp                                                      { SP }
  | '\x29' lf                                               { CLOSE_PAREN }
  | sp (['0'-'9']+ as lxm)                                  { NUM (int_of_string lxm) }
  | "\"_none_\" 1 0 -1"                                     { NOPOS }
  | '"' ([^'"']+ as filename) '"'                           { FILENAME filename }
  | "type("                                                 { TYPE }
  | "call("                                                 { CALL }
  | "ident(\x0A\x20\x20def\x20" ([^' ']+ as name)           { DEF name }
  | "ident(\x0A\x20\x20int_ref\x20"([^' ']+ as name)        { INT_REF name }
  | "ident(\x0A\x20\x20ext_ref\x20" ([^'\x0A']+ as name)    { EXT_REF name }
  | line_of_data                                            {
      let buffer = Buffer.create 100 in
      data buffer lexbuf
    }
  | "--"                                                    { NOPOS }
and data buffer = parse
  | eof                           { raise (Failure "Unexpected end of file") }
  | line_of_data                  {
      Buffer.add_char buffer '\n';
      data buffer lexbuf;
    }
  | lf                            { DATA (Buffer.contents buffer) }
  | _                             {
      Buffer.add_string buffer (Lexing.lexeme lexbuf);
      data buffer lexbuf
    }

