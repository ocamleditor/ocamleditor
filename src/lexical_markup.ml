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


open Parser
open Printf
open Location
open Lexing
open Preferences

module Range = struct
  let range_of_loc {loc_start; loc_end; _} =
    loc_start.pos_cnum - loc_start.pos_bol,
    loc_end.pos_cnum - loc_end.pos_bol

  let (!!) = range_of_loc

  let rec range_intersection ((a1, b1) as r1) ((a2, b2) as r2) =
    (* a1 <= b1 && a2 <= b2 *)
    if a1 <= a2 then
      if b2 <= b1 then Some r2
      else if a2 <= b1 then Some (a2, b1)
      else None
    else range_intersection r2 r1;;

  let (^^^) = range_intersection
end

open Range

let parse pref =
  let tags = pref.Settings_t.editor_tags in
  let bgcolor_highlight = preferences#get.editor_mark_occurrences_bg_color in
  let span_highlight text =
    String.concat "" ["<span bgcolor='"; ?? bgcolor_highlight; "'>"; (Glib.Markup.escape_text text); "</span>"]
  in
  let span (highlight, tagname) =
    match List.find_opt (fun t -> t.Settings_t.name = tagname) tags with
    | Some t ->
        let weight    = sprintf " font_weight='%d'" t.weight in
        let style     = match t.style with `ITALIC -> " font_style='italic'" | _ -> "" in
        let underline = match t.underline with `NONE -> "" | _ -> " underline='single'" in
        let bgcolor   = if highlight then " bgcolor='" ^ (?? bgcolor_highlight) ^"'" else "" in
        String.concat "" ["<span color='"; ??(t.color); "'"; weight; style; underline; bgcolor; ">"]
    | _ -> assert false
  in
  let span = Miscellanea.Memo.create span in
  fun ?(highlights=[]) text ->
    let buffer = Lexing.from_string text in
    let out = Buffer.create (String.length text) in
    let pos = ref 0 in
    let pending = ref false in
    let close_pending () =
      if !pending then begin
        Buffer.add_string out "</span>";
        pending := false
      end;
    in
    let tag = ref "" in
    let last = ref EOF in
    let last_but_one = ref EOF in
    let in_record = ref false in
    let in_record_label = ref false in
    let in_annotation = ref false in
    try
      while true do
        try
          let token = Lexer.token buffer in
          let (lstart, lstop) as range = Lexing.lexeme_start buffer, Lexing.lexeme_end buffer in
          let lexeme = Lexing.lexeme buffer in
          tag := begin
            match token with
            | AMPERAMPER
            | AMPERSAND
            | OR
            | BARBAR
            | DO | DONE
            | DOWNTO
            | ELSE
            | FOR
            | IF
            | LAZY
            | MATCH
            | THEN
            | TO
            | TRY
            | WHEN
            | WHILE
            | WITH
              -> "control"
            | AND
            | ANDOP _
            | AS
            | BAR
            | CLASS
            | CONSTRAINT
            | EXCEPTION
            | EXTERNAL
            | FUN
            | FUNCTION
            | FUNCTOR
            | INHERIT
            | LET
            | LETOP _
            | METHOD
            | MODULE
            | MUTABLE
            | NONREC
            | PRIVATE
            | REC
            | TYPE
            | VAL
            | VIRTUAL
              -> "define"
            | IN | INITIALIZER | NEW | OF -> "define"
            | BEGIN
            | END
            | INCLUDE
            | OBJECT
            | OPEN
            | SIG
            | STRUCT
              -> "structure"
            | CHAR _
            | STRING _
            | QUOTED_STRING_EXPR _ | QUOTED_STRING_ITEM _
              -> "char"
            (*| BACKQUOTE*)
            | INFIXOP0 _
            | INFIXOP1 _
            | INFIXOP2 _
            | INFIXOP3 _
            | INFIXOP4 _
            | PREFIXOP _
            | HASH | HASHOP _
            | BANG
              -> "infix"
            | LABEL _
            | OPTLABEL _
            | QUESTION
            | TILDE
              -> "label"
            | UIDENT _ | BACKQUOTE -> "uident"
            | LIDENT _ when !in_annotation -> "annotation"
            | LIDENT _ ->
                begin match !last with
                | QUESTION | TILDE -> "label"
                | BACKQUOTE -> "number"
                | _ -> "lident"
                (*(* TODO:  *)
                  | _, LBRACE, _, _ when !in_record -> "record_label"
                  | _, MUTABLE, _, _ when !in_record -> "record_label"
                  | _, WITH, _, _ when !in_record -> "record_label"
                  | _, SEMI, _, _ when !in_record -> "record_label"
                  | _, DOT, _, _ when !in_record && !in_record_label -> "record_label"
                  | _, LPAREN, _, _ ->
                  (match !last_but_one with
                    | _, (QUESTION | TILDE), _, _ -> "label"
                    | _ -> (if lexeme = "failwith" || lexeme = "raise" || lexeme = "invalid_arg" then "custom" else "lident"))
                  | last ->
                  (if lexeme = "failwith" || lexeme = "raise" || lexeme = "invalid_arg" then "custom" else tag_lident last)*)
                end
            | COLON -> ""
            (*begin match !last with
              _, LIDENT _, lstart, lstop ->
                if lstop = start then
                  tb#apply_tag_by_name "label" ~start:(tpos lstart) ~stop:(tpos stop);
                ""
              | _ -> ""
              end*)
            | INT _ | FLOAT _  | TRUE | FALSE -> "number"
            | LBRACE -> in_record := true; in_record_label := true; "symbol"
            | RBRACE -> in_record := false; in_record_label := false; "symbol"
            | EQUAL when !in_record -> in_record_label := false; "symbol"
            | LPAREN | RPAREN | LBRACKET | BARRBRACKET | LBRACKETLESS | LBRACKETGREATER | GREATERRBRACKET
            | LBRACELESS | GREATERRBRACE | LBRACKETBAR | LESSMINUS
            | EQUAL | PLUS | MINUS | STAR | QUOTE | SEMI | SEMISEMI | MINUSGREATER
            | COMMA | DOT | DOTDOT | COLONCOLON | COLONEQUAL | UNDERSCORE
            | PLUSDOT | MINUSDOT | LESS | GREATER
            | PLUSEQ | PERCENT
            | COLONGREATER
            | DOTOP _
              -> "symbol"
            | LBRACKETAT | LBRACKETPERCENT | LBRACKETPERCENTPERCENT | LBRACKETATAT | LBRACKETATATAT
              -> in_annotation:= true; "annotation"
            | RBRACKET -> if !in_annotation then (in_annotation := false; "annotation") else "symbol"
            | ASSERT -> "custom"
            | DOCSTRING _ | COMMENT _ -> "comment" (* Lexer ignores comments *)
            | EOL -> ""
            | EOF -> raise End_of_file
          end;

          Buffer.add_string out (Glib.Markup.escape_text (String.sub text !pos (lstart - !pos)));
          close_pending();
          (*  *)
          let add with_span =
            match List.fold_left (fun acc h -> match h ^^^ range with None -> acc | x -> x) None highlights with
            | Some (a, b) when a < b ->
                if with_span then begin
                  if a = lstart && b = lstop then begin
                    Buffer.add_string out (span (true, !tag));
                    Buffer.add_string out (Glib.Markup.escape_text lexeme);
                  end else begin (* TODO: Other cases non implemented *)
                    Buffer.add_string out (span (false, !tag));
                    Buffer.add_string out (Glib.Markup.escape_text lexeme);
                  end
                end else begin
                  if a = lstart && b = lstop then begin
                    let before = String.sub text lstart (b - lstart) in
                    let after = String.sub text b (lstop - b) in
                    if before <> "" then (Buffer.add_string out (span_highlight before));
                    if after <> "" then (Buffer.add_string out (Glib.Markup.escape_text after));
                  end else begin
                    let before = String.sub text lstart (a - lstart) in
                    let middle = String.sub text a (b - a) in
                    let after = String.sub text b (lstop - b) in
                    if before <> "" then (Buffer.add_string out (Glib.Markup.escape_text before););
                    if middle <> "" then (Buffer.add_string out (span_highlight middle));
                    if after <> "" then (Buffer.add_string out (Glib.Markup.escape_text after););
                  end
                end
            | _ ->
                if with_span then (Buffer.add_string out (span (false, !tag)));
                Buffer.add_string out (Glib.Markup.escape_text lexeme);
          in
          (*  *)
          if !tag <> "" then begin
            add true;
            if !tag <> "char" then (Buffer.add_string out "</span>") else (pending := true);
          end else begin
            add false;
          end;
          pos := lstart + (String.length lexeme);
          last_but_one := !last;
          last := token;
        with Lexer.Error (err, _) ->
          begin
            let open Lexer in
            match err with
            | Illegal_character c -> printf "Illegal_character %C\n%!" c
            | Illegal_escape (s, sopt) -> printf "Illegal_escape \n%!"
            | Reserved_sequence (s, sopt) -> printf "Reserved_sequence \n%!"
            | Unterminated_comment loc -> printf "Unterminated_comment \n%!"
            | Unterminated_string -> printf "Unterminated_string \n%!"
            | Unterminated_string_in_comment (l1, l2) -> printf "Unterminated_string_in_comment \n%!"
            | Empty_character_literal -> printf "Empty_character_literal \n%!"
            | Keyword_as_label s -> printf "Keyword_as_label \n%!"
            | Invalid_literal s -> printf "Invalid_literal \n%!"
            | Invalid_directive (s, sopt) -> printf "Invalid_directive \n%!"
          end
      done;
      close_pending();
      Buffer.contents out
    with
    | End_of_file ->
        let lexeme = (String.sub text !pos (String.length text - !pos)) in
        (* TODO: consider highlights *)
        Buffer.add_string out (Glib.Markup.escape_text lexeme);
        close_pending();
        (*    Lexer.comments ()
              |> List.iter begin fun (c, loc) ->
              let _, line_start, char_start = Location.get_pos_info (loc.loc_start) in
              let _, line_stop, char_stop = Location.get_pos_info (loc.loc_end) in
              Printf.printf "comment = %d,%d -- %d,%d\n%!" line_start char_start line_stop char_stop;
              end;*)
        Buffer.contents out
    | (Lexer.Error (error, _)) as ex ->
        close_pending();
        Buffer.contents out
    | ex ->
        close_pending();
        (printf "Lexical_markup: %s\n%!" (Printexc.to_string ex));
        ""
