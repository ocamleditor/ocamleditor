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
%}

%token <int> NUM
%token <string> FILENAME
%token <string> DATA
%token <string> DEF
%token <string> INT_REF
%token <string> EXT_REF
%token SP LF CLOSE_PAREN TYPE CALL NOPOS EOF

%start file
%type <Oe.annot_block list> file
%%
file:
  | /* empty */                           { [] }
  | block file                            { $1 :: $2 }
;
block:
  | position SP position LF annotation    { { annot_start = $1; annot_stop = $3; annot_annotations = $5 } }
;
position:
  | FILENAME NUM NUM NUM                  { { annot_fname = $1;
                                              annot_lnum  = $2;
                                              annot_bol   = $3;
                                              annot_cnum  = $4 } }
;
ident:
  | EXT_REF                               { Ext_ref $1 }
  | INT_REF SP position SP position       { Int_ref ($1, $3, $5) }
  | DEF SP position SP NOPOS              { Def ($1, $3, None) }
  | DEF SP position SP position           { Def ($1, $3, Some $5) }
;
annotation:
  | ident LF CLOSE_PAREN annotation       { $1 :: $4 }
  | TYPE DATA CLOSE_PAREN annotation      { Type $2 :: $4 }
  | CALL DATA CLOSE_PAREN annotation      { (match $2 with "stack" -> Stack | _ -> Tail) :: $4 }
  | /* empty */                           { [] }
;





