  (* $Id: lexical.ml,v 1.14.2.1 2003/03/13 11:47:20 garrigue Exp $ *)

open Parser
open Str
open Printf

let multi_space = regexp "\\( \\( +\\)\\)\\|(\\*\\*\\(\\*+\\)\\|(\\*\\* \\|(\\* \\|(\\*\\|\\*)"

let tags, colors = List.split Preferences.preferences#get.Preferences.pref_tags

let tags = ref tags
let colors = ref colors

(* Initialization *)
let init_tags ?(tags=(!tags)) ?(colors=(!colors))
    ?(ocamldoc_paragraph_enabled=Oe_config.ocamldoc_paragraph_bgcolor_enabled)
    ?(ocamldoc_paragraph_bgcolor_1=(Preferences.preferences#get.Preferences.pref_ocamldoc_paragraph_bgcolor_1))
    ?(ocamldoc_paragraph_bgcolor_2=(Preferences.preferences#get.Preferences.pref_ocamldoc_paragraph_bgcolor_2))
    (tb : #GText.buffer) =
  let table = new GText.tag_table tb#tag_table in
  List.iter2
    begin fun tagname (col, weight, style, undline, scale, (bg_default, bg_color)) ->
      if tagname <> "highlight_current_line" then begin
        begin
          match table#lookup tagname with
            | None -> ()
            | Some t -> table#remove t
        end;
        let properties = [`FOREGROUND_GDK (GDraw.color col); `WEIGHT weight; `STYLE style; `UNDERLINE undline; `SCALE scale] in
        let properties = if bg_default then properties else (`BACKGROUND_GDK (GDraw.color bg_color)) :: properties in
        let tag = tb#create_tag ~name:tagname properties in
        if tagname = "ocamldoc" then begin
          if ocamldoc_paragraph_enabled then begin
            Gaux.may ocamldoc_paragraph_bgcolor_2 ~f:begin fun bg2 ->
              Gmisclib.Util.set_tag_paragraph_background tag bg2;
            end;
          end;
          Gaux.may (table#lookup "ocamldoc-paragraph") ~f:table#remove;
          let tag = tb#create_tag ~name:"ocamldoc-paragraph"
            [`FOREGROUND_GDK (GDraw.color col); `WEIGHT weight; `STYLE style; `UNDERLINE undline; `PIXELS_BELOW_LINES 1; `PIXELS_ABOVE_LINES 1] in
          if ocamldoc_paragraph_enabled then begin
            Gaux.may ocamldoc_paragraph_bgcolor_1 ~f:begin fun bg1 ->
              Gmisclib.Util.set_tag_paragraph_background tag bg1;
            end
          end
        end
      end
    end tags colors;;
  (*begin
    match table#lookup "error" with
      | None -> ()
      | Some t -> table#remove t
  end;
  ignore(tb#create_tag ~name:"error" [`FOREGROUND "red"; `WEIGHT `BOLD])*)

(* Line_offset *)
let line_starts s =
  let len = String.length s in
  let rec next_line ~accu pos =
    if pos >= len then accu else
    let res = try 1 + String.index_from s pos '\n' with Not_found -> 0 in
    if res = 0 then accu else
    next_line ~accu:(res :: accu) res
  in
  next_line ~accu:[0] 0

let rec line_offset ~lines pos =
  match lines with [] -> invalid_arg "Lexical.line_offset"
  | last :: prev ->
      if pos >= last then (List.length prev, pos - last)
      else line_offset ~lines:prev pos

let tpos ~(start : GText.iter) ~lines pos =
  let l, c = line_offset ~lines pos in
  let result =
    if l = 0 then begin
      let li = start#line_index in
      let result = start#set_line_index (c + li) in
      result
    end else
      (start#forward_lines l)#set_line_index c
  in
  result

let tag_lident = function
  | _, METHOD, _, _ | _, PRIVATE, _, _ -> "method_name_def"
  | _, IN, _, _ | _, INITIALIZER, _, _ | _, NEW, _, _ | _, OF, _, _ -> "lident"
  | "define", _, _, _  -> "name_def"
  | _ -> "lident"

(* Tagging *)

let tag ?start ?stop (tb : GText.buffer) =
  let start = Gaux.default tb#start_iter ~opt:start
  and stop = Gaux.default tb#end_iter ~opt:stop in
  (* Se start e stop sono all'interno di commenti allora prendo come start e stop
     l'inizio del primo commento e la fine del secondo. *)
  let text = tb#get_text () in
  let text = Glib.Convert.convert_with_fallback ~fallback:"?" ~from_codeset:"UTF-8" ~to_codeset:Oe_config.ocaml_codeset text in
  let global_comments = Comments.scan text in
  let start = match Comments.enclosing global_comments start#offset with
    | None -> start
    | Some (x, y) ->
      tb#get_iter_at_char x in
  let stop = match Comments.enclosing global_comments stop#offset with
    | None -> stop
    | Some (x, y) ->
      tb#get_iter_at_char y in
  (*  *)
  let u_text = tb#get_text ~start ~stop () in
  let lines = line_starts u_text in
  let tpos = tpos ~start ~lines in
  let i_text = (Glib.Convert.convert_with_fallback ~fallback:"?" ~from_codeset:"UTF-8" ~to_codeset:Oe_config.ocaml_codeset u_text) in
  let buffer = Lexing.from_string i_text in
  let extra_bytes = ref 0 in
  let comments = Comments.scan_utf8 u_text in
  let succ_comments = ref comments in
  let last = ref ("", EOF, 0, 0) in
  let last_but_one = ref ("", EOF, 0, 0) in
  let in_record = ref false in
  let in_record_label = ref false in
  List.iter begin function
    | tagname when tagname <> "highlight_current_line" -> tb#remove_tag_by_name tagname ~start ~stop
    | _ -> ()
  end !tags;
  try
    while true do
      try
        let token = Lexer.token buffer in
        let lstart = Lexing.lexeme_start buffer in
        let lstop = Lexing.lexeme_end buffer in
        let length = lstop - lstart in
        let u_lstart, u_lstop =
          (Glib.Utf8.offset_to_pos ~pos:0 ~off:lstart u_text),
          (Glib.Utf8.offset_to_pos ~pos:0 ~off:lstop u_text)
        in
        let u_length = u_lstop - u_lstart in
        (*printf "(%d, %d) (%d, %d)\n%!" lstart lstop u_lstart u_lstop;*)
        let lexeme = String.sub u_text u_lstart u_length in
        let start1 = lstart + !extra_bytes in
        let _, succ, start = Comments.partition !succ_comments start1  in
        succ_comments := succ;
        let extra_bytes_in_comments = start - start1 in
        let extra_bytes_in_lexeme = u_length - length in
        let stop = start + u_length in
        extra_bytes := !extra_bytes + extra_bytes_in_lexeme + extra_bytes_in_comments;
        let tag =
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
          | HASH
              -> "infix"
          | LABEL _
          | OPTLABEL _
          | QUESTION
          | TILDE
              -> "label"
          | UIDENT _ | BACKQUOTE -> "uident"
          | LIDENT _ ->
              begin match !last with
                | _, (QUESTION | TILDE), _, _ -> "label"
                | _, BACKQUOTE, _, _ -> "number"
                (* TODO:  *)
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
                  (if lexeme = "failwith" || lexeme = "raise" || lexeme = "invalid_arg" then "custom" else tag_lident last)
              end
          | COLON ->
              begin match !last with
                _, LIDENT _, lstart, lstop ->
                  if lstop = start then
                    tb#apply_tag_by_name "label" ~start:(tpos lstart) ~stop:(tpos stop);
                  ""
              | _ -> ""
              end
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
        in
        if tag <> "" then begin
          tb#apply_tag_by_name tag ~start:(tpos start) ~stop:(tpos stop);
        end;
        last_but_one := !last;
        last := (tag, token, start, stop)
      with Lexer.Error _ -> ()
    done;
  with
    | End_of_file ->
      (* comments *)
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
      end (match global_comments with Comments.Locale x -> x | _ -> failwith "Lexical: Comments.Utf8")
    | (Lexer.Error (error, _)) as ex -> ()
    | ex -> (printf "Lexical: %s\n%!" (Printexc.to_string ex))















