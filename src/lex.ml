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

open Lexing
open Parser
open Printf

type result = {
  lexeme : string;
  start: int;
  length: int
}

(** Funzione generale di analisi lessicale.*)
let analyse ?(utf8=true) ?pend ?(error=ignore) text f =
  let pend = ref pend in
  let text = if utf8 then Glib.Convert.convert_with_fallback ~fallback:"?"
      ~from_codeset:"UTF-8" ~to_codeset:Oe_config.ocaml_codeset text
    else text in
  let lexbuf = Lexing.from_string text in
  try
    while true do
      let token = match !pend with None -> Lexer.token lexbuf | Some t -> t in
      if token = EOF then raise End_of_file
      else
        let start = Lexing.lexeme_start lexbuf in
        let stop = Lexing.lexeme_end lexbuf in
        let length = stop - start in
        let lexeme = String.sub lexbuf.lex_buffer start length in
        pend := f ~token ~lexeme ~start ~length ~lexbuf;
    done
  with
    | End_of_file -> ()
    | Lexer.Error (err, loc) -> begin
      error (err, loc)
    end

(** scan *)
let scan ?(utf8=true) ?(ignore_lexer_error=true) text f =
  let text = if utf8 then Glib.Convert.convert_with_fallback ~fallback:"?"
      ~from_codeset:"UTF-8" ~to_codeset:Oe_config.ocaml_codeset text
    else text in
  let lexbuf = Lexing.from_string text in
  try
    while true do
      let token = Lexer.token lexbuf in
      if token = EOF then raise End_of_file
      else
        let start = Lexing.lexeme_start lexbuf in
        let stop = Lexing.lexeme_end lexbuf in
        f ~token ~start ~stop;
    done
  with End_of_file -> () | Lexer.Error _ when ignore_lexer_error -> ()

(** Moduli aperti con "open" nel testo. *)
let paths_opened text =
  let paths = ref [] in
  analyse text begin fun ~token ~lexeme ~start ~length ~lexbuf ->
    match token with
      | OPEN ->
        let path = ref "" in
        let next_token = ref token in
        while
          next_token := Lexer.token lexbuf;
          match !next_token with
            | UIDENT _ | DOT ->
              path := (!path)^(Lexing.lexeme lexbuf);
              true
            | _ -> false
        do () done;
        paths := !path :: !paths;
        Some !next_token
      | _ -> None
  end;
  (List.rev !paths)

(** Tutte le stringhe. *)
let strings text =
  let strings = ref [] in
  analyse text begin fun ~token ~lexeme ~start ~length ~lexbuf ->
    match token with
      | STRING s ->
        strings := {lexeme = s; start = start; length = length} :: !strings;
        None
      | _ -> None
  end;
  (*List.rev*) !strings

(** in_string *)
let in_string ?(utf8=true) text =
  let strings = ref [] in
  analyse ~utf8 text begin fun ~token ~lexeme ~start ~length ~lexbuf ->
    match token with
      | STRING s ->
        strings := (start, start + length) :: !strings;
        None
      | _ -> None
  end;
  let rec find strings pos =
    match strings with
      | (start, stop) :: rest ->
        if start < pos && pos < stop then true else (find rest pos)
      | [] -> false
  in find !strings






























