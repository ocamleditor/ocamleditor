(* Auto-generated from "merlin.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type type_expression = { value: string }

type message = { value: string }

type type_expression_answer = 
    Return of type_expression
  | Failure of message
  | Error of message
  | Exception of message


type tail =  No | Position | Call 

type pos = { line: int; col: int }

type type_enclosing_value = {
  te_start: pos;
  te_stop: pos;
  te_type: string;
  te_tail: tail
}

type type_enclosing = { value: type_enclosing_value list }

type type_enclosing_answer = 
    Return of type_enclosing
  | Failure of message
  | Error of message
  | Exception of message


type range = { file: string option; start: pos; stop: pos }

type list_modules = { value: string list }

type list_modules_answer = 
    Return of list_modules
  | Failure of message
  | Error of message
  | Exception of message


type label = { name: string; typ: string }

type entry = { name: string; kind: string; desc: string; info: string }

type enclosing = { value: range list }

type enclosing_answer = 
    Return of enclosing
  | Failure of message
  | Error of message
  | Exception of message


type document = { value: string }

type document_answer = 
    Return of document
  | Failure of message
  | Error of message
  | Exception of message


type application_context = { argument_type: string; labels: label list }

type complete_prefix_value = {
  context: (string * application_context) option;
  entries: entry list
}

type complete_prefix = { value: complete_prefix_value }

type complete_prefix_answer = 
    Return of complete_prefix
  | Failure of message
  | Error of message
  | Exception of message


type case_analysis = { value: (range * string) }

type case_analysis_answer = 
    Return of case_analysis
  | Failure of message
  | Error of message
  | Exception of message

