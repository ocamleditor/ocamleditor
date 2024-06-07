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


type range = Merlin_t.range = { file: string option; start: pos; stop: pos }

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


let write_type_expression : _ -> type_expression -> _ = (
  fun ob (x : type_expression) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"value\":";
    (
      Yojson.Safe.write_string
    )
      ob x.value;
    Buffer.add_char ob '}';
)
let string_of_type_expression ?(len = 1024) x =
  let ob = Buffer.create len in
  write_type_expression ob x;
  Buffer.contents ob
let read_type_expression = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_value = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_value := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_value := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            value = (match !field_value with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "value");
          }
         : type_expression)
      )
)
let type_expression_of_string s =
  read_type_expression (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_message : _ -> message -> _ = (
  fun ob (x : message) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"value\":";
    (
      Yojson.Safe.write_string
    )
      ob x.value;
    Buffer.add_char ob '}';
)
let string_of_message ?(len = 1024) x =
  let ob = Buffer.create len in
  write_message ob x;
  Buffer.contents ob
let read_message = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_value = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_value := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_value := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            value = (match !field_value with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "value");
          }
         : message)
      )
)
let message_of_string s =
  read_message (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_type_expression_answer : _ -> type_expression_answer -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter (Atdgen_runtime.Json_adapter.restore_type_field "class") (
    fun ob (x : type_expression_answer) ->
      match x with
        | Return x ->
          Buffer.add_string ob "<\"return\":";
          (
            write_type_expression
          ) ob x;
          Buffer.add_char ob '>'
        | Failure x ->
          Buffer.add_string ob "<\"failure\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Error x ->
          Buffer.add_string ob "<\"error\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Exception x ->
          Buffer.add_string ob "<\"exception\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
  )
)
let string_of_type_expression_answer ?(len = 1024) x =
  let ob = Buffer.create len in
  write_type_expression_answer ob x;
  Buffer.contents ob
let read_type_expression_answer = (
  Atdgen_runtime.Oj_run.read_with_adapter (Atdgen_runtime.Json_adapter.normalize_type_field "class") (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            match Yojson.Safe.read_ident p lb with
              | "return" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_type_expression
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Return x : type_expression_answer)
              | "failure" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Failure x : type_expression_answer)
              | "error" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Error x : type_expression_answer)
              | "exception" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Exception x : type_expression_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "return" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_type_expression
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Return x : type_expression_answer)
              | "failure" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Failure x : type_expression_answer)
              | "error" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Error x : type_expression_answer)
              | "exception" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Exception x : type_expression_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let type_expression_answer_of_string s =
  read_type_expression_answer (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tail : _ -> tail -> _ = (
  fun ob (x : tail) ->
    match x with
      | No -> Buffer.add_string ob "<\"no\">"
      | Position -> Buffer.add_string ob "<\"position\">"
      | Call -> Buffer.add_string ob "<\"call\">"
)
let string_of_tail ?(len = 1024) x =
  let ob = Buffer.create len in
  write_tail ob x;
  Buffer.contents ob
let read_tail = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "no" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (No : tail)
            | "position" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Position : tail)
            | "call" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Call : tail)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "no" ->
              (No : tail)
            | "position" ->
              (Position : tail)
            | "call" ->
              (Call : tail)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let tail_of_string s =
  read_tail (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_pos : _ -> pos -> _ = (
  fun ob (x : pos) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"line\":";
    (
      Yojson.Safe.write_int
    )
      ob x.line;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"col\":";
    (
      Yojson.Safe.write_int
    )
      ob x.col;
    Buffer.add_char ob '}';
)
let string_of_pos ?(len = 1024) x =
  let ob = Buffer.create len in
  write_pos ob x;
  Buffer.contents ob
let read_pos = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_line = ref (None) in
    let field_col = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 3 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' then (
                  1
                )
                else (
                  -1
                )
              )
            | 4 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_line := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 1 ->
            field_col := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 3 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 4 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_line := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 1 ->
              field_col := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            line = (match !field_line with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "line");
            col = (match !field_col with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "col");
          }
         : pos)
      )
)
let pos_of_string s =
  read_pos (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_type_enclosing_value : _ -> type_enclosing_value -> _ = (
  fun ob (x : type_enclosing_value) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"start\":";
    (
      write_pos
    )
      ob x.te_start;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"end\":";
    (
      write_pos
    )
      ob x.te_stop;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"type\":";
    (
      Yojson.Safe.write_string
    )
      ob x.te_type;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"tail\":";
    (
      write_tail
    )
      ob x.te_tail;
    Buffer.add_char ob '}';
)
let string_of_type_enclosing_value ?(len = 1024) x =
  let ob = Buffer.create len in
  write_type_enclosing_value ob x;
  Buffer.contents ob
let read_type_enclosing_value = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_te_start = ref (None) in
    let field_te_stop = ref (None) in
    let field_te_type = ref (None) in
    let field_te_tail = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 3 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' then (
                  1
                )
                else (
                  -1
                )
              )
            | 4 -> (
                if String.unsafe_get s pos = 't' then (
                  match String.unsafe_get s (pos+1) with
                    | 'a' -> (
                        if String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'l' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | 'y' -> (
                        if String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 't' then (
                  0
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_te_start := (
              Some (
                (
                  read_pos
                ) p lb
              )
            );
          | 1 ->
            field_te_stop := (
              Some (
                (
                  read_pos
                ) p lb
              )
            );
          | 2 ->
            field_te_type := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 3 ->
            field_te_tail := (
              Some (
                (
                  read_tail
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 3 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 4 -> (
                  if String.unsafe_get s pos = 't' then (
                    match String.unsafe_get s (pos+1) with
                      | 'a' -> (
                          if String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'l' then (
                            3
                          )
                          else (
                            -1
                          )
                        )
                      | 'y' -> (
                          if String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' then (
                            2
                          )
                          else (
                            -1
                          )
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_te_start := (
                Some (
                  (
                    read_pos
                  ) p lb
                )
              );
            | 1 ->
              field_te_stop := (
                Some (
                  (
                    read_pos
                  ) p lb
                )
              );
            | 2 ->
              field_te_type := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 3 ->
              field_te_tail := (
                Some (
                  (
                    read_tail
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            te_start = (match !field_te_start with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "te_start");
            te_stop = (match !field_te_stop with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "te_stop");
            te_type = (match !field_te_type with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "te_type");
            te_tail = (match !field_te_tail with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "te_tail");
          }
         : type_enclosing_value)
      )
)
let type_enclosing_value_of_string s =
  read_type_enclosing_value (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__type_enclosing_value_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_type_enclosing_value
  )
)
let string_of__type_enclosing_value_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__type_enclosing_value_list ob x;
  Buffer.contents ob
let read__type_enclosing_value_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_type_enclosing_value
  )
)
let _type_enclosing_value_list_of_string s =
  read__type_enclosing_value_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_type_enclosing : _ -> type_enclosing -> _ = (
  fun ob (x : type_enclosing) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"value\":";
    (
      write__type_enclosing_value_list
    )
      ob x.value;
    Buffer.add_char ob '}';
)
let string_of_type_enclosing ?(len = 1024) x =
  let ob = Buffer.create len in
  write_type_enclosing ob x;
  Buffer.contents ob
let read_type_enclosing = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_value = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_value := (
              Some (
                (
                  read__type_enclosing_value_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_value := (
                Some (
                  (
                    read__type_enclosing_value_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            value = (match !field_value with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "value");
          }
         : type_enclosing)
      )
)
let type_enclosing_of_string s =
  read_type_enclosing (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_type_enclosing_answer : _ -> type_enclosing_answer -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter (Atdgen_runtime.Json_adapter.restore_type_field "class") (
    fun ob (x : type_enclosing_answer) ->
      match x with
        | Return x ->
          Buffer.add_string ob "<\"return\":";
          (
            write_type_enclosing
          ) ob x;
          Buffer.add_char ob '>'
        | Failure x ->
          Buffer.add_string ob "<\"failure\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Error x ->
          Buffer.add_string ob "<\"error\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Exception x ->
          Buffer.add_string ob "<\"exception\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
  )
)
let string_of_type_enclosing_answer ?(len = 1024) x =
  let ob = Buffer.create len in
  write_type_enclosing_answer ob x;
  Buffer.contents ob
let read_type_enclosing_answer = (
  Atdgen_runtime.Oj_run.read_with_adapter (Atdgen_runtime.Json_adapter.normalize_type_field "class") (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            match Yojson.Safe.read_ident p lb with
              | "return" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_type_enclosing
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Return x : type_enclosing_answer)
              | "failure" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Failure x : type_enclosing_answer)
              | "error" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Error x : type_enclosing_answer)
              | "exception" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Exception x : type_enclosing_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "return" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_type_enclosing
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Return x : type_enclosing_answer)
              | "failure" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Failure x : type_enclosing_answer)
              | "error" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Error x : type_enclosing_answer)
              | "exception" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Exception x : type_enclosing_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let type_enclosing_answer_of_string s =
  read_type_enclosing_answer (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_option = (
  Atdgen_runtime.Oj_run.write_option (
    Yojson.Safe.write_string
  )
)
let string_of__string_option ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_option ob x;
  Buffer.contents ob
let read__string_option = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _string_option_of_string s =
  read__string_option (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_range : _ -> range -> _ = (
  fun ob (x : range) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    (match x.file with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"file\":";
      (
        Yojson.Safe.write_string
      )
        ob x;
    );
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"start\":";
    (
      write_pos
    )
      ob x.start;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"end\":";
    (
      write_pos
    )
      ob x.stop;
    Buffer.add_char ob '}';
)
let string_of_range ?(len = 1024) x =
  let ob = Buffer.create len in
  write_range ob x;
  Buffer.contents ob
let read_range = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_file = ref (None) in
    let field_start = ref (None) in
    let field_stop = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 3 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' then (
                  2
                )
                else (
                  -1
                )
              )
            | 4 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 't' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_file := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            )
          | 1 ->
            field_start := (
              Some (
                (
                  read_pos
                ) p lb
              )
            );
          | 2 ->
            field_stop := (
              Some (
                (
                  read_pos
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 3 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 4 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 't' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_file := (
                  Some (
                    (
                      Atdgen_runtime.Oj_run.read_string
                    ) p lb
                  )
                );
              )
            | 1 ->
              field_start := (
                Some (
                  (
                    read_pos
                  ) p lb
                )
              );
            | 2 ->
              field_stop := (
                Some (
                  (
                    read_pos
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            file = !field_file;
            start = (match !field_start with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "start");
            stop = (match !field_stop with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "stop");
          }
         : range)
      )
)
let range_of_string s =
  read_range (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_list = (
  Atdgen_runtime.Oj_run.write_list (
    Yojson.Safe.write_string
  )
)
let string_of__string_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_list ob x;
  Buffer.contents ob
let read__string_list = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_string
  )
)
let _string_list_of_string s =
  read__string_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_list_modules : _ -> list_modules -> _ = (
  fun ob (x : list_modules) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"value\":";
    (
      write__string_list
    )
      ob x.value;
    Buffer.add_char ob '}';
)
let string_of_list_modules ?(len = 1024) x =
  let ob = Buffer.create len in
  write_list_modules ob x;
  Buffer.contents ob
let read_list_modules = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_value = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_value := (
              Some (
                (
                  read__string_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_value := (
                Some (
                  (
                    read__string_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            value = (match !field_value with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "value");
          }
         : list_modules)
      )
)
let list_modules_of_string s =
  read_list_modules (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_list_modules_answer : _ -> list_modules_answer -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter (Atdgen_runtime.Json_adapter.restore_type_field "class") (
    fun ob (x : list_modules_answer) ->
      match x with
        | Return x ->
          Buffer.add_string ob "<\"return\":";
          (
            write_list_modules
          ) ob x;
          Buffer.add_char ob '>'
        | Failure x ->
          Buffer.add_string ob "<\"failure\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Error x ->
          Buffer.add_string ob "<\"error\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Exception x ->
          Buffer.add_string ob "<\"exception\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
  )
)
let string_of_list_modules_answer ?(len = 1024) x =
  let ob = Buffer.create len in
  write_list_modules_answer ob x;
  Buffer.contents ob
let read_list_modules_answer = (
  Atdgen_runtime.Oj_run.read_with_adapter (Atdgen_runtime.Json_adapter.normalize_type_field "class") (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            match Yojson.Safe.read_ident p lb with
              | "return" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_list_modules
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Return x : list_modules_answer)
              | "failure" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Failure x : list_modules_answer)
              | "error" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Error x : list_modules_answer)
              | "exception" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Exception x : list_modules_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "return" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_list_modules
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Return x : list_modules_answer)
              | "failure" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Failure x : list_modules_answer)
              | "error" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Error x : list_modules_answer)
              | "exception" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Exception x : list_modules_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let list_modules_answer_of_string s =
  read_list_modules_answer (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_label : _ -> label -> _ = (
  fun ob (x : label) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"type\":";
    (
      Yojson.Safe.write_string
    )
      ob x.typ;
    Buffer.add_char ob '}';
)
let string_of_label ?(len = 1024) x =
  let ob = Buffer.create len in
  write_label ob x;
  Buffer.contents ob
let read_label = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_name = ref (None) in
    let field_typ = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 4 then (
            match String.unsafe_get s pos with
              | 'n' -> (
                  if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 't' -> (
                  if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_name := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_typ := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 4 then (
              match String.unsafe_get s pos with
                | 'n' -> (
                    if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                      0
                    )
                    else (
                      -1
                    )
                  )
                | 't' -> (
                    if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' then (
                      1
                    )
                    else (
                      -1
                    )
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_name := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_typ := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            name = (match !field_name with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "name");
            typ = (match !field_typ with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "typ");
          }
         : label)
      )
)
let label_of_string s =
  read_label (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_entry : _ -> entry -> _ = (
  fun ob (x : entry) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"kind\":";
    (
      Yojson.Safe.write_string
    )
      ob x.kind;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"desc\":";
    (
      Yojson.Safe.write_string
    )
      ob x.desc;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"info\":";
    (
      Yojson.Safe.write_string
    )
      ob x.info;
    Buffer.add_char ob '}';
)
let string_of_entry ?(len = 1024) x =
  let ob = Buffer.create len in
  write_entry ob x;
  Buffer.contents ob
let read_entry = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_name = ref (None) in
    let field_kind = ref (None) in
    let field_desc = ref (None) in
    let field_info = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 4 then (
            match String.unsafe_get s pos with
              | 'd' -> (
                  if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'c' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 'i' -> (
                  if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'f' && String.unsafe_get s (pos+3) = 'o' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 'k' -> (
                  if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'd' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 'n' -> (
                  if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_name := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_kind := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 2 ->
            field_desc := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 3 ->
            field_info := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 4 then (
              match String.unsafe_get s pos with
                | 'd' -> (
                    if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'c' then (
                      2
                    )
                    else (
                      -1
                    )
                  )
                | 'i' -> (
                    if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'f' && String.unsafe_get s (pos+3) = 'o' then (
                      3
                    )
                    else (
                      -1
                    )
                  )
                | 'k' -> (
                    if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'd' then (
                      1
                    )
                    else (
                      -1
                    )
                  )
                | 'n' -> (
                    if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                      0
                    )
                    else (
                      -1
                    )
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_name := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_kind := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 2 ->
              field_desc := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 3 ->
              field_info := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            name = (match !field_name with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "name");
            kind = (match !field_kind with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "kind");
            desc = (match !field_desc with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "desc");
            info = (match !field_info with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "info");
          }
         : entry)
      )
)
let entry_of_string s =
  read_entry (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__range_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_range
  )
)
let string_of__range_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__range_list ob x;
  Buffer.contents ob
let read__range_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_range
  )
)
let _range_list_of_string s =
  read__range_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_enclosing : _ -> enclosing -> _ = (
  fun ob (x : enclosing) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"value\":";
    (
      write__range_list
    )
      ob x.value;
    Buffer.add_char ob '}';
)
let string_of_enclosing ?(len = 1024) x =
  let ob = Buffer.create len in
  write_enclosing ob x;
  Buffer.contents ob
let read_enclosing = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_value = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_value := (
              Some (
                (
                  read__range_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_value := (
                Some (
                  (
                    read__range_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            value = (match !field_value with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "value");
          }
         : enclosing)
      )
)
let enclosing_of_string s =
  read_enclosing (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_enclosing_answer : _ -> enclosing_answer -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter (Atdgen_runtime.Json_adapter.restore_type_field "class") (
    fun ob (x : enclosing_answer) ->
      match x with
        | Return x ->
          Buffer.add_string ob "<\"return\":";
          (
            write_enclosing
          ) ob x;
          Buffer.add_char ob '>'
        | Failure x ->
          Buffer.add_string ob "<\"failure\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Error x ->
          Buffer.add_string ob "<\"error\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Exception x ->
          Buffer.add_string ob "<\"exception\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
  )
)
let string_of_enclosing_answer ?(len = 1024) x =
  let ob = Buffer.create len in
  write_enclosing_answer ob x;
  Buffer.contents ob
let read_enclosing_answer = (
  Atdgen_runtime.Oj_run.read_with_adapter (Atdgen_runtime.Json_adapter.normalize_type_field "class") (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            match Yojson.Safe.read_ident p lb with
              | "return" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_enclosing
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Return x : enclosing_answer)
              | "failure" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Failure x : enclosing_answer)
              | "error" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Error x : enclosing_answer)
              | "exception" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Exception x : enclosing_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "return" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_enclosing
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Return x : enclosing_answer)
              | "failure" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Failure x : enclosing_answer)
              | "error" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Error x : enclosing_answer)
              | "exception" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Exception x : enclosing_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let enclosing_answer_of_string s =
  read_enclosing_answer (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_document : _ -> document -> _ = (
  fun ob (x : document) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"value\":";
    (
      Yojson.Safe.write_string
    )
      ob x.value;
    Buffer.add_char ob '}';
)
let string_of_document ?(len = 1024) x =
  let ob = Buffer.create len in
  write_document ob x;
  Buffer.contents ob
let read_document = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_value = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_value := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_value := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            value = (match !field_value with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "value");
          }
         : document)
      )
)
let document_of_string s =
  read_document (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_document_answer : _ -> document_answer -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter (Atdgen_runtime.Json_adapter.restore_type_field "class") (
    fun ob (x : document_answer) ->
      match x with
        | Return x ->
          Buffer.add_string ob "<\"return\":";
          (
            write_document
          ) ob x;
          Buffer.add_char ob '>'
        | Failure x ->
          Buffer.add_string ob "<\"failure\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Error x ->
          Buffer.add_string ob "<\"error\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Exception x ->
          Buffer.add_string ob "<\"exception\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
  )
)
let string_of_document_answer ?(len = 1024) x =
  let ob = Buffer.create len in
  write_document_answer ob x;
  Buffer.contents ob
let read_document_answer = (
  Atdgen_runtime.Oj_run.read_with_adapter (Atdgen_runtime.Json_adapter.normalize_type_field "class") (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            match Yojson.Safe.read_ident p lb with
              | "return" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_document
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Return x : document_answer)
              | "failure" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Failure x : document_answer)
              | "error" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Error x : document_answer)
              | "exception" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Exception x : document_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "return" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_document
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Return x : document_answer)
              | "failure" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Failure x : document_answer)
              | "error" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Error x : document_answer)
              | "exception" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Exception x : document_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let document_answer_of_string s =
  read_document_answer (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__label_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_label
  )
)
let string_of__label_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__label_list ob x;
  Buffer.contents ob
let read__label_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_label
  )
)
let _label_list_of_string s =
  read__label_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_application_context : _ -> application_context -> _ = (
  fun ob (x : application_context) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"argument_type\":";
    (
      Yojson.Safe.write_string
    )
      ob x.argument_type;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"labels\":";
    (
      write__label_list
    )
      ob x.labels;
    Buffer.add_char ob '}';
)
let string_of_application_context ?(len = 1024) x =
  let ob = Buffer.create len in
  write_application_context ob x;
  Buffer.contents ob
let read_application_context = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_argument_type = ref (None) in
    let field_labels = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 6 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | 13 -> (
                if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 'y' && String.unsafe_get s (pos+11) = 'p' && String.unsafe_get s (pos+12) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_argument_type := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_labels := (
              Some (
                (
                  read__label_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 6 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 13 -> (
                  if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 'y' && String.unsafe_get s (pos+11) = 'p' && String.unsafe_get s (pos+12) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_argument_type := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_labels := (
                Some (
                  (
                    read__label_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            argument_type = (match !field_argument_type with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "argument_type");
            labels = (match !field_labels with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "labels");
          }
         : application_context)
      )
)
let application_context_of_string s =
  read_application_context (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_application_context_option = (
  Atdgen_runtime.Oj_run.write_option (
    fun ob x ->
      Buffer.add_char ob '(';
      (let x, _ = x in
      (
        Yojson.Safe.write_string
      ) ob x
      );
      Buffer.add_char ob ',';
      (let _, x = x in
      (
        write_application_context
      ) ob x
      );
      Buffer.add_char ob ')';
  )
)
let string_of__string_application_context_option ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_application_context_option ob x;
  Buffer.contents ob
let read__string_application_context_option = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_string
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read_application_context
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_string
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            read_application_context
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _string_application_context_option_of_string s =
  read__string_application_context_option (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__entry_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_entry
  )
)
let string_of__entry_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__entry_list ob x;
  Buffer.contents ob
let read__entry_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_entry
  )
)
let _entry_list_of_string s =
  read__entry_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_complete_prefix_value : _ -> complete_prefix_value -> _ = (
  fun ob (x : complete_prefix_value) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    (match x.context with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"context\":";
      (
        fun ob x ->
          Buffer.add_char ob '(';
          (let x, _ = x in
          (
            Yojson.Safe.write_string
          ) ob x
          );
          Buffer.add_char ob ',';
          (let _, x = x in
          (
            write_application_context
          ) ob x
          );
          Buffer.add_char ob ')';
      )
        ob x;
    );
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"entries\":";
    (
      write__entry_list
    )
      ob x.entries;
    Buffer.add_char ob '}';
)
let string_of_complete_prefix_value ?(len = 1024) x =
  let ob = Buffer.create len in
  write_complete_prefix_value ob x;
  Buffer.contents ob
let read_complete_prefix_value = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_context = ref (None) in
    let field_entries = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 7 then (
            match String.unsafe_get s pos with
              | 'c' -> (
                  if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'x' && String.unsafe_get s (pos+6) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 'e' -> (
                  if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_context := (
                Some (
                  (
                    fun p lb ->
                      Yojson.Safe.read_space p lb;
                      let std_tuple = Yojson.Safe.start_any_tuple p lb in
                      let len = ref 0 in
                      let end_of_tuple = ref false in
                      (try
                        let x0 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_string
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x1 =
                          let x =
                            (
                              read_application_context
                            ) p lb
                          in
                          incr len;
                          (try
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          with Yojson.End_of_tuple -> end_of_tuple := true);
                          x
                        in
                        if not !end_of_tuple then (
                          try
                            while true do
                              Yojson.Safe.skip_json p lb;
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                            done
                          with Yojson.End_of_tuple -> ()
                        );
                        (x0, x1)
                      with Yojson.End_of_tuple ->
                        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                  ) p lb
                )
              );
            )
          | 1 ->
            field_entries := (
              Some (
                (
                  read__entry_list
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 7 then (
              match String.unsafe_get s pos with
                | 'c' -> (
                    if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'x' && String.unsafe_get s (pos+6) = 't' then (
                      0
                    )
                    else (
                      -1
                    )
                  )
                | 'e' -> (
                    if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 's' then (
                      1
                    )
                    else (
                      -1
                    )
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_context := (
                  Some (
                    (
                      fun p lb ->
                        Yojson.Safe.read_space p lb;
                        let std_tuple = Yojson.Safe.start_any_tuple p lb in
                        let len = ref 0 in
                        let end_of_tuple = ref false in
                        (try
                          let x0 =
                            let x =
                              (
                                Atdgen_runtime.Oj_run.read_string
                              ) p lb
                            in
                            incr len;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                            x
                          in
                          let x1 =
                            let x =
                              (
                                read_application_context
                              ) p lb
                            in
                            incr len;
                            (try
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                            with Yojson.End_of_tuple -> end_of_tuple := true);
                            x
                          in
                          if not !end_of_tuple then (
                            try
                              while true do
                                Yojson.Safe.skip_json p lb;
                                Yojson.Safe.read_space p lb;
                                Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                              done
                            with Yojson.End_of_tuple -> ()
                          );
                          (x0, x1)
                        with Yojson.End_of_tuple ->
                          Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                    ) p lb
                  )
                );
              )
            | 1 ->
              field_entries := (
                Some (
                  (
                    read__entry_list
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            context = !field_context;
            entries = (match !field_entries with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "entries");
          }
         : complete_prefix_value)
      )
)
let complete_prefix_value_of_string s =
  read_complete_prefix_value (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_complete_prefix : _ -> complete_prefix -> _ = (
  fun ob (x : complete_prefix) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"value\":";
    (
      write_complete_prefix_value
    )
      ob x.value;
    Buffer.add_char ob '}';
)
let string_of_complete_prefix ?(len = 1024) x =
  let ob = Buffer.create len in
  write_complete_prefix ob x;
  Buffer.contents ob
let read_complete_prefix = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_value = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_value := (
              Some (
                (
                  read_complete_prefix_value
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_value := (
                Some (
                  (
                    read_complete_prefix_value
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            value = (match !field_value with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "value");
          }
         : complete_prefix)
      )
)
let complete_prefix_of_string s =
  read_complete_prefix (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_complete_prefix_answer : _ -> complete_prefix_answer -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter (Atdgen_runtime.Json_adapter.restore_type_field "class") (
    fun ob (x : complete_prefix_answer) ->
      match x with
        | Return x ->
          Buffer.add_string ob "<\"return\":";
          (
            write_complete_prefix
          ) ob x;
          Buffer.add_char ob '>'
        | Failure x ->
          Buffer.add_string ob "<\"failure\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Error x ->
          Buffer.add_string ob "<\"error\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Exception x ->
          Buffer.add_string ob "<\"exception\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
  )
)
let string_of_complete_prefix_answer ?(len = 1024) x =
  let ob = Buffer.create len in
  write_complete_prefix_answer ob x;
  Buffer.contents ob
let read_complete_prefix_answer = (
  Atdgen_runtime.Oj_run.read_with_adapter (Atdgen_runtime.Json_adapter.normalize_type_field "class") (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            match Yojson.Safe.read_ident p lb with
              | "return" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_complete_prefix
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Return x : complete_prefix_answer)
              | "failure" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Failure x : complete_prefix_answer)
              | "error" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Error x : complete_prefix_answer)
              | "exception" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Exception x : complete_prefix_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "return" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_complete_prefix
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Return x : complete_prefix_answer)
              | "failure" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Failure x : complete_prefix_answer)
              | "error" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Error x : complete_prefix_answer)
              | "exception" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Exception x : complete_prefix_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let complete_prefix_answer_of_string s =
  read_complete_prefix_answer (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_case_analysis : _ -> case_analysis -> _ = (
  fun ob (x : case_analysis) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"value\":";
    (
      fun ob x ->
        Buffer.add_char ob '(';
        (let x, _ = x in
        (
          write_range
        ) ob x
        );
        Buffer.add_char ob ',';
        (let _, x = x in
        (
          Yojson.Safe.write_string
        ) ob x
        );
        Buffer.add_char ob ')';
    )
      ob x.value;
    Buffer.add_char ob '}';
)
let string_of_case_analysis ?(len = 1024) x =
  let ob = Buffer.create len in
  write_case_analysis ob x;
  Buffer.contents ob
let read_case_analysis = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_value = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_value := (
              Some (
                (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            read_range
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_string
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            if len = 5 && String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'e' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_value := (
                Some (
                  (
                    fun p lb ->
                      Yojson.Safe.read_space p lb;
                      let std_tuple = Yojson.Safe.start_any_tuple p lb in
                      let len = ref 0 in
                      let end_of_tuple = ref false in
                      (try
                        let x0 =
                          let x =
                            (
                              read_range
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x1 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_string
                            ) p lb
                          in
                          incr len;
                          (try
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          with Yojson.End_of_tuple -> end_of_tuple := true);
                          x
                        in
                        if not !end_of_tuple then (
                          try
                            while true do
                              Yojson.Safe.skip_json p lb;
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                            done
                          with Yojson.End_of_tuple -> ()
                        );
                        (x0, x1)
                      with Yojson.End_of_tuple ->
                        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            value = (match !field_value with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "value");
          }
         : case_analysis)
      )
)
let case_analysis_of_string s =
  read_case_analysis (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_case_analysis_answer : _ -> case_analysis_answer -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter (Atdgen_runtime.Json_adapter.restore_type_field "class") (
    fun ob (x : case_analysis_answer) ->
      match x with
        | Return x ->
          Buffer.add_string ob "<\"return\":";
          (
            write_case_analysis
          ) ob x;
          Buffer.add_char ob '>'
        | Failure x ->
          Buffer.add_string ob "<\"failure\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Error x ->
          Buffer.add_string ob "<\"error\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
        | Exception x ->
          Buffer.add_string ob "<\"exception\":";
          (
            write_message
          ) ob x;
          Buffer.add_char ob '>'
  )
)
let string_of_case_analysis_answer ?(len = 1024) x =
  let ob = Buffer.create len in
  write_case_analysis_answer ob x;
  Buffer.contents ob
let read_case_analysis_answer = (
  Atdgen_runtime.Oj_run.read_with_adapter (Atdgen_runtime.Json_adapter.normalize_type_field "class") (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      match Yojson.Safe.start_any_variant p lb with
        | `Edgy_bracket -> (
            match Yojson.Safe.read_ident p lb with
              | "return" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_case_analysis
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Return x : case_analysis_answer)
              | "failure" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Failure x : case_analysis_answer)
              | "error" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Error x : case_analysis_answer)
              | "exception" ->
                Atdgen_runtime.Oj_run.read_until_field_value p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_gt p lb;
                (Exception x : case_analysis_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Double_quote -> (
            match Yojson.Safe.finish_string p lb with
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
        | `Square_bracket -> (
            match Atdgen_runtime.Oj_run.read_string p lb with
              | "return" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_case_analysis
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Return x : case_analysis_answer)
              | "failure" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Failure x : case_analysis_answer)
              | "error" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Error x : case_analysis_answer)
              | "exception" ->
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_comma p lb;
                Yojson.Safe.read_space p lb;
                let x = (
                    read_message
                  ) p lb
                in
                Yojson.Safe.read_space p lb;
                Yojson.Safe.read_rbr p lb;
                (Exception x : case_analysis_answer)
              | x ->
                Atdgen_runtime.Oj_run.invalid_variant_tag p x
          )
  )
)
let case_analysis_answer_of_string s =
  read_case_analysis_answer (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
