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


open Parser
open Printf

let tag_lident = function
  | _, METHOD, _, _ | _, PRIVATE, _, _ -> "method_name_def"
  | _, IN, _, _ | _, INITIALIZER, _, _ | _, NEW, _, _ | _, OF, _, _ -> "lident"
  | "define", _, _, _  -> "name_def"
  | _ -> "lident";;

let parse pref =
  let tags = pref.Preferences.pref_tags in
  let span tagname =
    match List.assoc tagname tags with
      | `NAME color, weight, style, underline, _, _ ->
        let weight    = match weight with `BOLD -> " font_weight='bold'" | _ -> "" in
        let style     = match style with `ITALIC -> " font_style='italic'" | _ -> "" in
        let underline = match underline with `LOW -> " underline='low'" | _ -> "" in
        sprintf "<span color='%s'%s%s%s>" color weight style underline
      | _ -> assert false
  in
  let span = Miscellanea.Memo.fast ~f:span in
  fun text ->
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
    try
      while true do
        try
          let token = Lexer.token buffer in
          let lstart = Lexing.lexeme_start buffer in
          (*let lstop = Lexing.lexeme_end buffer in*)
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
            | METHOD
            | MODULE
            | MUTABLE
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
                -> "char"
            (*| BACKQUOTE*)
            | INFIXOP0 _
            | INFIXOP1 _
            | INFIXOP2 _
            | INFIXOP3 _
            | INFIXOP4 _
            | PREFIXOP _
            | SHARP
                -> "infix"
            | LABEL _
            | OPTLABEL _
            | QUESTION
            | TILDE
                -> "label"
            | UIDENT _ | BACKQUOTE -> "uident"
            | LIDENT _ ->
                begin match !last with
                  | QUESTION | TILDE -> "label"
                  | BACKQUOTE -> "number"
                  | _ -> ""
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
            | LPAREN | RPAREN | LBRACKET | BARRBRACKET| RBRACKET | LBRACKETLESS | GREATERRBRACKET
            | LBRACELESS | GREATERRBRACE | LBRACKETBAR | LESSMINUS
            | EQUAL | PLUS | MINUS | STAR | QUOTE | SEMI | SEMISEMI | MINUSGREATER
            | COMMA | DOT | DOTDOT | COLONCOLON | COLONEQUAL (*| LBRACE*) (*| RBRACE*) | UNDERSCORE
              -> "symbol"
            | ASSERT -> "custom"
            | EOF -> raise End_of_file
            | _ -> ""
          end;
          Buffer.add_string out (Glib.Markup.escape_text (String.sub text !pos (lstart - !pos)));
          close_pending();
          if !tag <> "" then begin
            Buffer.add_string out (span !tag);
            Buffer.add_string out (Glib.Markup.escape_text lexeme);
            if !tag <> "char" then (Buffer.add_string out "</span>") else (pending := true);
          end else (Buffer.add_string out (Glib.Markup.escape_text lexeme));
          pos := lstart + (String.length lexeme);
          last_but_one := !last;
          last := token;
        with Lexer.Error _ -> ()
      done;
      close_pending();
      Buffer.contents out
    with
      | End_of_file ->
        let lexeme = (String.sub text !pos (String.length text - !pos)) in
        Buffer.add_string out (Glib.Markup.escape_text lexeme);
        close_pending();
        Buffer.contents out
        (*(* comments *)
        List.iter begin fun (b, e, _, ocamldoc) ->
          if not ocamldoc then begin
            let ms = Miscellanea.Search.all multi_space begin fun ~pos ~matched_string:mat ->
              Miscellanea.Search.Append (pos, pos + String.length mat, mat)
            end (String.sub u_text b (e - b)) in
            let (*b = b and*) e = e - 2 in
            let tag = "comment" in
            tb#apply_tag_by_name tag ~start:(tpos b) ~stop:(tpos e);
            List.iter begin fun (b1, e1, _) ->
              tb#apply_tag_by_name tag ~start:(tpos (b + b1)) ~stop:(tpos (b + e1))
            end ms
          end
        end (match comments with Comments.Utf8 x -> x | _ -> failwith "Lexical: Comments.Locale");
        (* ocamldoc and ocamldoc-paragraph *)
        List.iter begin fun (b, e, ocamldoc) ->
          if ocamldoc then begin
            let start = tb#get_iter (`OFFSET b) in
            let stop = tb#get_iter (`OFFSET e) in
            let tag, start =
              let iter = start#backward_line in
              if iter#ends_line
              then ("ocamldoc-paragraph", start#set_line_index 0)
              else ("ocamldoc", start#set_line_index 0) (* start *)
            in
            tb#apply_tag_by_name tag ~start ~stop;
          end
        end (match global_comments with Comments.Locale x -> x | _ -> failwith "Lexical: Comments.Utf8")*)
      | (Lexer.Error (error, _)) as ex ->
        close_pending();
        Buffer.contents out
      | ex ->
        close_pending();
        (printf "Lexical_markup: %s\n%!" (Printexc.to_string ex));
        ""
