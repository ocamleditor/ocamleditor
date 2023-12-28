(* Auto-generated from "merlin.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type type_expression = Merlin_t.type_expression = { value: string }

type message = Merlin_t.message = { value: string }

type type_expression_answer = Merlin_t.type_expression_answer = 
    Return of type_expression
  | Failure of message
  | Error of message
  | Exception of message


type tail = Merlin_t.tail =  No | Position | Call 

type pos = Merlin_t.pos = { line: int; col: int }

type type_enclosing_value = Merlin_t.type_enclosing_value = {
  te_start: pos;
  te_stop: pos;
  te_type: string;
  te_tail: tail
}

type type_enclosing = Merlin_t.type_enclosing = {
  value: type_enclosing_value list
}

type type_enclosing_answer = Merlin_t.type_enclosing_answer = 
    Return of type_enclosing
  | Failure of message
  | Error of message
  | Exception of message


type range = Merlin_t.range = { start: pos; stop: pos }

type list_modules = Merlin_t.list_modules = { value: string list }

type list_modules_answer = Merlin_t.list_modules_answer = 
    Return of list_modules
  | Failure of message
  | Error of message
  | Exception of message


type label = Merlin_t.label = { name: string; typ: string }

type entry = Merlin_t.entry = {
  name: string;
  kind: string;
  desc: string;
  info: string
}

type enclosing = Merlin_t.enclosing = { value: range list }

type enclosing_answer = Merlin_t.enclosing_answer = 
    Return of enclosing
  | Failure of message
  | Error of message
  | Exception of message


type document = Merlin_t.document = { value: string }

type document_answer = Merlin_t.document_answer = 
    Return of document
  | Failure of message
  | Error of message
  | Exception of message


type application_context = Merlin_t.application_context = {
  argument_type: string;
  labels: label list
}

type complete_prefix_value = Merlin_t.complete_prefix_value = {
  context: (string * application_context) option;
  entries: entry list
}

type complete_prefix = Merlin_t.complete_prefix = {
  value: complete_prefix_value
}

type complete_prefix_answer = Merlin_t.complete_prefix_answer = 
    Return of complete_prefix
  | Failure of message
  | Error of message
  | Exception of message


type case_analysis = Merlin_t.case_analysis = { value: (range * string) }

type case_analysis_answer = Merlin_t.case_analysis_answer = 
    Return of case_analysis
  | Failure of message
  | Error of message
  | Exception of message


val write_type_expression :
  Buffer.t -> type_expression -> unit
(** Output a JSON value of type {!type:type_expression}. *)

val string_of_type_expression :
  ?len:int -> type_expression -> string
(** Serialize a value of type {!type:type_expression}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_type_expression :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> type_expression
(** Input JSON data of type {!type:type_expression}. *)

val type_expression_of_string :
  string -> type_expression
(** Deserialize JSON data of type {!type:type_expression}. *)

val write_message :
  Buffer.t -> message -> unit
(** Output a JSON value of type {!type:message}. *)

val string_of_message :
  ?len:int -> message -> string
(** Serialize a value of type {!type:message}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_message :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> message
(** Input JSON data of type {!type:message}. *)

val message_of_string :
  string -> message
(** Deserialize JSON data of type {!type:message}. *)

val write_type_expression_answer :
  Buffer.t -> type_expression_answer -> unit
(** Output a JSON value of type {!type:type_expression_answer}. *)

val string_of_type_expression_answer :
  ?len:int -> type_expression_answer -> string
(** Serialize a value of type {!type:type_expression_answer}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_type_expression_answer :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> type_expression_answer
(** Input JSON data of type {!type:type_expression_answer}. *)

val type_expression_answer_of_string :
  string -> type_expression_answer
(** Deserialize JSON data of type {!type:type_expression_answer}. *)

val write_tail :
  Buffer.t -> tail -> unit
(** Output a JSON value of type {!type:tail}. *)

val string_of_tail :
  ?len:int -> tail -> string
(** Serialize a value of type {!type:tail}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_tail :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tail
(** Input JSON data of type {!type:tail}. *)

val tail_of_string :
  string -> tail
(** Deserialize JSON data of type {!type:tail}. *)

val write_pos :
  Buffer.t -> pos -> unit
(** Output a JSON value of type {!type:pos}. *)

val string_of_pos :
  ?len:int -> pos -> string
(** Serialize a value of type {!type:pos}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_pos :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pos
(** Input JSON data of type {!type:pos}. *)

val pos_of_string :
  string -> pos
(** Deserialize JSON data of type {!type:pos}. *)

val write_type_enclosing_value :
  Buffer.t -> type_enclosing_value -> unit
(** Output a JSON value of type {!type:type_enclosing_value}. *)

val string_of_type_enclosing_value :
  ?len:int -> type_enclosing_value -> string
(** Serialize a value of type {!type:type_enclosing_value}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_type_enclosing_value :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> type_enclosing_value
(** Input JSON data of type {!type:type_enclosing_value}. *)

val type_enclosing_value_of_string :
  string -> type_enclosing_value
(** Deserialize JSON data of type {!type:type_enclosing_value}. *)

val write_type_enclosing :
  Buffer.t -> type_enclosing -> unit
(** Output a JSON value of type {!type:type_enclosing}. *)

val string_of_type_enclosing :
  ?len:int -> type_enclosing -> string
(** Serialize a value of type {!type:type_enclosing}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_type_enclosing :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> type_enclosing
(** Input JSON data of type {!type:type_enclosing}. *)

val type_enclosing_of_string :
  string -> type_enclosing
(** Deserialize JSON data of type {!type:type_enclosing}. *)

val write_type_enclosing_answer :
  Buffer.t -> type_enclosing_answer -> unit
(** Output a JSON value of type {!type:type_enclosing_answer}. *)

val string_of_type_enclosing_answer :
  ?len:int -> type_enclosing_answer -> string
(** Serialize a value of type {!type:type_enclosing_answer}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_type_enclosing_answer :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> type_enclosing_answer
(** Input JSON data of type {!type:type_enclosing_answer}. *)

val type_enclosing_answer_of_string :
  string -> type_enclosing_answer
(** Deserialize JSON data of type {!type:type_enclosing_answer}. *)

val write_range :
  Buffer.t -> range -> unit
(** Output a JSON value of type {!type:range}. *)

val string_of_range :
  ?len:int -> range -> string
(** Serialize a value of type {!type:range}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_range :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> range
(** Input JSON data of type {!type:range}. *)

val range_of_string :
  string -> range
(** Deserialize JSON data of type {!type:range}. *)

val write_list_modules :
  Buffer.t -> list_modules -> unit
(** Output a JSON value of type {!type:list_modules}. *)

val string_of_list_modules :
  ?len:int -> list_modules -> string
(** Serialize a value of type {!type:list_modules}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_list_modules :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> list_modules
(** Input JSON data of type {!type:list_modules}. *)

val list_modules_of_string :
  string -> list_modules
(** Deserialize JSON data of type {!type:list_modules}. *)

val write_list_modules_answer :
  Buffer.t -> list_modules_answer -> unit
(** Output a JSON value of type {!type:list_modules_answer}. *)

val string_of_list_modules_answer :
  ?len:int -> list_modules_answer -> string
(** Serialize a value of type {!type:list_modules_answer}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_list_modules_answer :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> list_modules_answer
(** Input JSON data of type {!type:list_modules_answer}. *)

val list_modules_answer_of_string :
  string -> list_modules_answer
(** Deserialize JSON data of type {!type:list_modules_answer}. *)

val write_label :
  Buffer.t -> label -> unit
(** Output a JSON value of type {!type:label}. *)

val string_of_label :
  ?len:int -> label -> string
(** Serialize a value of type {!type:label}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_label :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> label
(** Input JSON data of type {!type:label}. *)

val label_of_string :
  string -> label
(** Deserialize JSON data of type {!type:label}. *)

val write_entry :
  Buffer.t -> entry -> unit
(** Output a JSON value of type {!type:entry}. *)

val string_of_entry :
  ?len:int -> entry -> string
(** Serialize a value of type {!type:entry}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_entry :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> entry
(** Input JSON data of type {!type:entry}. *)

val entry_of_string :
  string -> entry
(** Deserialize JSON data of type {!type:entry}. *)

val write_enclosing :
  Buffer.t -> enclosing -> unit
(** Output a JSON value of type {!type:enclosing}. *)

val string_of_enclosing :
  ?len:int -> enclosing -> string
(** Serialize a value of type {!type:enclosing}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_enclosing :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> enclosing
(** Input JSON data of type {!type:enclosing}. *)

val enclosing_of_string :
  string -> enclosing
(** Deserialize JSON data of type {!type:enclosing}. *)

val write_enclosing_answer :
  Buffer.t -> enclosing_answer -> unit
(** Output a JSON value of type {!type:enclosing_answer}. *)

val string_of_enclosing_answer :
  ?len:int -> enclosing_answer -> string
(** Serialize a value of type {!type:enclosing_answer}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_enclosing_answer :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> enclosing_answer
(** Input JSON data of type {!type:enclosing_answer}. *)

val enclosing_answer_of_string :
  string -> enclosing_answer
(** Deserialize JSON data of type {!type:enclosing_answer}. *)

val write_document :
  Buffer.t -> document -> unit
(** Output a JSON value of type {!type:document}. *)

val string_of_document :
  ?len:int -> document -> string
(** Serialize a value of type {!type:document}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_document :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> document
(** Input JSON data of type {!type:document}. *)

val document_of_string :
  string -> document
(** Deserialize JSON data of type {!type:document}. *)

val write_document_answer :
  Buffer.t -> document_answer -> unit
(** Output a JSON value of type {!type:document_answer}. *)

val string_of_document_answer :
  ?len:int -> document_answer -> string
(** Serialize a value of type {!type:document_answer}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_document_answer :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> document_answer
(** Input JSON data of type {!type:document_answer}. *)

val document_answer_of_string :
  string -> document_answer
(** Deserialize JSON data of type {!type:document_answer}. *)

val write_application_context :
  Buffer.t -> application_context -> unit
(** Output a JSON value of type {!type:application_context}. *)

val string_of_application_context :
  ?len:int -> application_context -> string
(** Serialize a value of type {!type:application_context}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_application_context :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> application_context
(** Input JSON data of type {!type:application_context}. *)

val application_context_of_string :
  string -> application_context
(** Deserialize JSON data of type {!type:application_context}. *)

val write_complete_prefix_value :
  Buffer.t -> complete_prefix_value -> unit
(** Output a JSON value of type {!type:complete_prefix_value}. *)

val string_of_complete_prefix_value :
  ?len:int -> complete_prefix_value -> string
(** Serialize a value of type {!type:complete_prefix_value}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_complete_prefix_value :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> complete_prefix_value
(** Input JSON data of type {!type:complete_prefix_value}. *)

val complete_prefix_value_of_string :
  string -> complete_prefix_value
(** Deserialize JSON data of type {!type:complete_prefix_value}. *)

val write_complete_prefix :
  Buffer.t -> complete_prefix -> unit
(** Output a JSON value of type {!type:complete_prefix}. *)

val string_of_complete_prefix :
  ?len:int -> complete_prefix -> string
(** Serialize a value of type {!type:complete_prefix}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_complete_prefix :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> complete_prefix
(** Input JSON data of type {!type:complete_prefix}. *)

val complete_prefix_of_string :
  string -> complete_prefix
(** Deserialize JSON data of type {!type:complete_prefix}. *)

val write_complete_prefix_answer :
  Buffer.t -> complete_prefix_answer -> unit
(** Output a JSON value of type {!type:complete_prefix_answer}. *)

val string_of_complete_prefix_answer :
  ?len:int -> complete_prefix_answer -> string
(** Serialize a value of type {!type:complete_prefix_answer}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_complete_prefix_answer :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> complete_prefix_answer
(** Input JSON data of type {!type:complete_prefix_answer}. *)

val complete_prefix_answer_of_string :
  string -> complete_prefix_answer
(** Deserialize JSON data of type {!type:complete_prefix_answer}. *)

val write_case_analysis :
  Buffer.t -> case_analysis -> unit
(** Output a JSON value of type {!type:case_analysis}. *)

val string_of_case_analysis :
  ?len:int -> case_analysis -> string
(** Serialize a value of type {!type:case_analysis}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_case_analysis :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> case_analysis
(** Input JSON data of type {!type:case_analysis}. *)

val case_analysis_of_string :
  string -> case_analysis
(** Deserialize JSON data of type {!type:case_analysis}. *)

val write_case_analysis_answer :
  Buffer.t -> case_analysis_answer -> unit
(** Output a JSON value of type {!type:case_analysis_answer}. *)

val string_of_case_analysis_answer :
  ?len:int -> case_analysis_answer -> string
(** Serialize a value of type {!type:case_analysis_answer}
    into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_case_analysis_answer :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> case_analysis_answer
(** Input JSON data of type {!type:case_analysis_answer}. *)

val case_analysis_answer_of_string :
  string -> case_analysis_answer
(** Deserialize JSON data of type {!type:case_analysis_answer}. *)

