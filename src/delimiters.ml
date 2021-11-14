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


open Lexing
open Parser
open Printf

exception Exit_forward of int * int
exception Exit_backward

(** is_delimiter *)
let rec is_delimiter ?(utf8=true) text pos =
  let result = ref false in
  begin
    try
      Lex.scan ~utf8 text begin fun ~token ~start ~stop ->
        match token with
        | BEGIN | END | LET | IN | LPAREN | RPAREN | LBRACKET
        | RBRACKET | LBRACE | TRY | WITH | RBRACE | DO | DONE
          when start <= pos && pos <= stop ->
            result := true;
            raise End_of_file
        | _ -> ()
      end;
    with Lexer.Error _ (*as ex*) -> begin
        result := is_delimiter ~utf8 (String.sub text 1 (String.length text - 1)) (max (pos - 1) 0)
      end | Sys_error _ -> ()
  end;
  !result

(** find_backward *)
let rec find_backward delimiters delim n =
  match delimiters with
  | (`BEGIN, start, stop) :: rest when delim = `END ->
      if n = 0 then Some (start, stop) else (find_backward rest delim (n - 1))
  | (`END, start, stop) :: rest when delim = `END -> find_backward rest delim (n + 1)
  | (`LET, start, stop) :: rest when delim = `IN ->
      if n = 0 then Some (start, stop) else (find_backward rest delim (n - 1))
  | (`IN, start, stop) :: rest when delim = `IN -> find_backward rest delim (n + 1)
  | (`MATCH, start, stop) :: rest when delim = `WITH ->
      if n = 0 then None else (find_backward rest delim (n - 1));
  | (`TRY, start, stop) :: rest when delim = `WITH ->
      if n = 0 then Some (start, stop) else (find_backward rest delim (n - 1))
  | (`WITH, start, stop) :: rest when delim = `WITH -> find_backward rest delim (n + 1)
  | (`LPAREN, start, stop) :: rest when delim = `RPAREN ->
      if n = 0 then Some (start, stop) else (find_backward rest delim (n - 1))
  | (`RPAREN, start, stop) :: rest when delim = `RPAREN -> find_backward rest delim (n + 1)
  | (`LBRACKET, start, stop) :: rest when delim = `RBRACKET ->
      if n = 0 then Some (start, stop) else (find_backward rest delim (n - 1))
  | (`RBRACKET, start, stop) :: rest when delim = `RBRACKET -> find_backward rest delim (n + 1)
  | (`LBRACE, start, stop) :: rest when delim = `RBRACE ->
      if n = 0 then Some (start, stop) else (find_backward rest delim (n - 1))
  | (`RBRACE, start, stop) :: rest when delim = `RBRACE -> find_backward rest delim (n + 1)
  | (`DO, start, stop) :: rest when delim = `DONE ->
      if n = 0 then Some (start, stop) else (find_backward rest delim (n - 1))
  | (`DONE, start, stop) :: rest when delim = `DONE -> find_backward rest delim (n + 1)
  | [] -> None
  | _ :: rest -> find_backward rest delim n
;;

(** find *)
let find_match ?(utf8=true) text pos =
  let delimiters = ref [] in
  let fn = ref (-1) in
  let fd = ref `NONE in
  let fstart = ref 0 in
  let fstop = ref 0 in
  let f start stop tok =
    delimiters := (tok, start, stop) :: !delimiters;
    begin
      match tok with
      | `END | `DONE | `RPAREN | `RBRACKET | `RBRACE | `WITH | `IN when start <= pos && pos <= stop -> raise Exit_backward
      | `END when !fd = `BEGIN -> if !fn = 0 then raise (Exit_forward (start, stop)) else (decr fn)
      | `DONE when !fd = `DO -> if !fn = 0 then raise (Exit_forward (start, stop)) else (decr fn)
      | `RPAREN  when !fd = `LPAREN -> if !fn = 0 then raise (Exit_forward (start, stop)) else (decr fn)
      | `RBRACKET  when !fd = `LBRACKET -> if !fn = 0 then raise (Exit_forward (start, stop)) else (decr fn)
      | `RBRACE when !fd = `LBRACE -> if !fn = 0 then raise (Exit_forward (start, stop)) else (decr fn)
      | `WITH when !fd = `TRY -> if !fn = 0 then raise (Exit_forward (start, stop)) else (decr fn)
      | `IN when !fd = `LET -> if !fn = 0 then raise (Exit_forward (start, stop)) else (decr fn)
      | `BEGIN | `DO | `LPAREN | `LBRACKET | `LBRACE | `TRY | `LET when start <= pos && pos <= stop ->
          fn := 0;
          fd := tok;
          fstart := start;
          fstop := stop;
      | `MATCH when !fd = `TRY && !fn >= 0 -> incr fn
      | _ (* LEFT *) when !fd = tok && !fn >= 0 -> incr fn
      | _ (* LEFT *) when start < pos && pos <= stop -> raise Exit
      | _ (* LEFT *) -> ()
    end
  in
  try
    Lex.scan ~utf8 text begin fun ~token ~start ~stop ->
      match token with
      | BEGIN -> f start stop `BEGIN
      | END -> f start stop `END
      | LET -> f start stop `LET
      | IN -> f start stop `IN
      | LPAREN -> f start stop `LPAREN
      | RPAREN -> f start stop `RPAREN
      | LBRACKET -> f start stop `LBRACKET
      | RBRACKET -> f start stop `RBRACKET
      | LBRACE -> f start stop `LBRACE
      | RBRACE -> f start stop `RBRACE
      | TRY -> f start stop `TRY
      | MATCH -> f start stop `MATCH
      | WITH -> f start stop `WITH
      | DO -> f start stop `DO
      | DONE -> f start stop `DONE
      | _ -> ()
    end;
    None
  with
  | Exit -> None
  | Exit_forward (rstart, rstop) -> Some (!fstart, !fstop, rstart, rstop)
  | Exit_backward ->
      begin
        match !delimiters with
        | (delim, rstart, rstop) :: tl ->
            begin
              match find_backward tl delim 0 with None -> None
                                                | Some (lstart, lstop) -> Some (lstart, lstop, rstart, rstop)
            end
        | _ -> assert false
      end
  | ex -> (eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace ())); None;;

(** find_innermost_enclosing_delim *)
let find_innermost_enclosing_delim ?(utf8=true) text pos =
  let stack = ref [] in
  begin
    try
      Lex.scan ~utf8 text begin fun ~token ~start ~stop ->
        if pos < start then raise End_of_file else begin
          match token with
          | BEGIN | LPAREN | LBRACKET | LBRACE | DO (*| TRY | LET*) ->
              stack := (start, stop) :: !stack
          | END | RPAREN | RBRACKET | RBRACE | DONE (*| WITH | IN*) ->
              (match !stack with _ :: tl -> stack := tl | _ -> ())
          | _ -> ()
        end
      end;
    with Lexer.Error _ as ex -> begin
        printf "find_innermost_enclosing_delim: %s\n%!" (Printexc.to_string ex)
      end
  end;
  !stack;; (* [start, stop] of the left part of the innermost enclosing delimiters *)

let rec scan_folding_points = 
  let re_end_comment = Str.regexp "\\*)" in
  fun text ->
    let points = 
      ref [] (* Right end of the starting token * Whether the token has the closing token (aka has_end_token) *)
    in 
    let start = ref 0 in
    begin
      try
        Lex.scan ~utf8:true ~ignore_lexer_error:false text 
          begin fun ~token ~start ~stop ->
            match token with
            | METHOD -> points := (stop, false) :: !points;
            | VAL -> points := (stop, false) :: !points;
            | INITIALIZER -> points := (stop, false) :: !points;
            | END -> ()
            | OBJECT -> ()
            | STRUCT -> ()
            | SIG -> ()
            | BEGIN -> ()
            | LET -> points := (stop, false) :: !points;
            | IN -> points := (match !points with [] -> [] | a :: b -> b); (* Pops let...in *)
            | _ -> ()
          end;
      with
      | Lexer.Error (Lexer.Unterminated_string, _) -> ()
      | Lexer.Error (Lexer.Unterminated_comment _, _) -> ()
      | Lexer.Error (Lexer.Unterminated_string_in_comment _, _) -> ()
      | Lexer.Error _ as ex -> 
          Printf.eprintf "File \"delimiters.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
      | Sys_error _ -> begin
          Printf.eprintf "File \"delimiters.ml\": Sys_error\n%!";
          let pos = (Str.search_forward re_end_comment text 0) + 2 in
          let d, p = scan_folding_points (Str.string_after text pos) in
          points := d;
          start := pos;
        end
    end;
    !points, !start;;

let find_folding_point_end text =
  (* "text" begins just after the end of the starting token. The starting token 
     is not included in "text". *)
  let result = ref (0, false) in
  let stack_let = ref [] in
  let count_begin = ref 0 in
  try
    Lex.scan ~utf8:true text begin fun ~token ~start ~stop ->
      match token with
      | METHOD                    -> result := (start, false); raise Exit
      | INITIALIZER               -> result := (start, false); raise Exit
      | VAL                       -> result := (start, false); raise Exit
      | LET                       -> stack_let := start :: !stack_let
      | IN                        -> (try stack_let := List.tl !stack_let with Failure _ -> ())
      | SEMISEMI 
        when !stack_let = []      -> result := (stop, true); raise Exit
      | BEGIN                     -> incr count_begin
      | END when !count_begin > 0 -> decr count_begin
      | END                       -> result := (start, false); raise Exit
      | _                         -> ()
    end;
    begin 
      (* Return the position of the first "let" found which marks the end of our 
         "let" and which is at the bottom of the stack; if it doesn't exist, we 
         are on the last let-binding, so we return the end of the buffer. *)
      try !stack_let |> List.rev |> List.hd, false
      with Failure _ -> String.length text, false
    end
  with Exit -> !result
;;

(** scan_folding_points
  * on the assumption that the only construct that may not have the closing
  * delimiter is the global "let" binding.
*)
let rec scan_folding_points_old =
  let re_end_comment = Str.regexp "\\*)" in
  fun text ->
    let delim = ref [] in
    let start = ref 0 in
    let stack = ref [] in
    let pending_let = ref [] in
    let rec pop recursive stop =
      match !stack with
      | start :: tl when recursive && (match !pending_let with x :: _ -> x | _ -> assert false) ->
          (* Skip unclosed toplevel "let" bindings *)
          stack := tl;
          pending_let := List.tl !pending_let;
          pop recursive stop
      | start :: tl ->
          delim := (start, stop, true) :: !delim;
          stack := tl;
          pending_let := List.tl !pending_let
      | _ -> ()
    in
    begin
      try
        Lex.scan ~utf8:true ~ignore_lexer_error:false text begin fun ~token ~start ~stop ->
          match token with
          | BEGIN    -> pending_let := false :: !pending_let; stack := stop :: !stack;
          | LET      -> pending_let := true  :: !pending_let; stack := stop :: !stack;
          | DO       -> pending_let := false :: !pending_let; stack := stop :: !stack;
          | LBRACE   -> pending_let := false :: !pending_let; stack := stop :: !stack;
          | LBRACKET -> pending_let := false :: !pending_let; stack := stop :: !stack;
          | OBJECT   -> pending_let := false :: !pending_let; stack := stop :: !stack;
          | STRUCT   -> pending_let := false :: !pending_let; stack := stop :: !stack;
          | SIG      -> pending_let := false :: !pending_let; stack := stop :: !stack;
          | END      -> pop true start (*`BEGIN*)
          | IN       -> pop false start (*`LET*)
          | SEMISEMI -> pop false start (*`LET*)
          | DONE     -> pop true start (*`DO*)
          | RBRACE   -> pop true start (*`LBRACE*)
          | RBRACKET -> pop true start (*`LBRACKET*)
          | _ -> ()
        end;
      with
      | Lexer.Error (Lexer.Unterminated_string, _) -> ()
      | Lexer.Error (Lexer.Unterminated_comment _, _) -> ()
      | Lexer.Error (Lexer.Unterminated_string_in_comment _, _) -> ()
      | Lexer.Error _ as ex -> ()
      (*Printf.eprintf "File \"delimiters.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());*)
      | Sys_error _ -> begin
          let pos = (Str.search_forward re_end_comment text 0) + 2 in
          let d, p = scan_folding_points_old (Str.string_after text pos) in
          delim := d;
          start := pos;
        end
    end;
    List.iter (fun a -> delim := (a, -1, false) :: !delim) !stack;
    !delim, !start;;

(** scan *)
let scan text =
  let delim = ref [] in
  let stack = ref [] in
  let skip = ref 0 in
  begin
    try
      Lex.scan ~utf8:true text begin fun ~token ~start ~stop ->
        match token with
        | BEGIN | DO | TRY | LET | MATCH | OBJECT | STRUCT | SIG -> stack := start :: !stack;
        | END | DONE | WITH | IN ->
            if !skip = 0 then begin
              match !stack with
              | hd :: tl ->
                  delim := (hd, start) :: !delim;
                  stack := tl
              | _ -> ()
            end else (decr skip)
        | _ -> ()
      end;
    with
    | Lexer.Error _ as ex -> begin
        printf "Delimiters.scan: %s\n%!" (Printexc.to_string ex)
      end
    | Sys_error _ -> ()
  end;
  (*List.rev*) !delim;;






















