open Printf

module Log = Common.Log.Make(struct let prefix = "MERLIN" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `ERROR

let (//) = Filename.concat

let loop (f : in_channel -> unit) ((ic, _oc, _ec) as channels) =
  try while true do f ic done
  with End_of_file ->
    Unix.close_process_full channels |> ignore

let execute
    ?(continue_with=fun x -> x |> Yojson.Safe.prettify |> Log.println `INFO "%s")
    filename source_code command =
  let cwd = Sys.getcwd() in
  let filename = match Miscellanea.filename_relative cwd filename with Some path -> path | _ -> filename in
  let cmd_line = "ocamlmerlin" :: "server" :: command @ [ "-filename"; filename ] in
  Log.println `INFO "%s" (cmd_line |> String.concat " ");
  let (_, oc, _) as channels = Unix.open_process_full (cmd_line |> String.concat " ") (Unix.environment ()) in
  output_string oc source_code;
  close_out_noerr oc;
  Thread.create (loop (fun ic -> ic |> input_line |> continue_with)) channels |> ignore

let check_configuration ~filename ~source_code =
  [ "check-configuration" ]
  |> execute filename source_code

let errors ~filename ~source_code =
  [ "errors" ]
  |> execute filename source_code

let enclosing ~position:(line, col) ~filename ~source_code apply =
  let position = sprintf "%d:%d" line col in
  [ "enclosing"; "-position"; position ]
  |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.enclosing_answer_of_string json with
    | Return enclosing ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply enclosing.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let case_analysis ~(start : GText.iter) ~(stop : GText.iter) ~filename ~source_code apply =
  let start = sprintf "%d:%d" (start#line + 1) start#line_offset in
  let stop = sprintf "%d:%d" (stop#line + 1) stop#line_offset in
  [ "case-analysis"; "-start"; start; "-end"; stop ]
  |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.case_analysis_answer_of_string json with
    | Return case_analysis ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply case_analysis.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let complete_prefix ~position:(line, col) ~prefix ~filename ~source_code apply =
  let position = sprintf "%d:%d" line col in
  [ "complete-prefix"; "-position"; position; "-prefix"; sprintf "\"%s\"" prefix; "-doc true -types true" ]
  |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.complete_prefix_answer_of_string json with
    | Return complete ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply complete.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let expand_prefix ~position:(line, col) ~prefix ~filename ~source_code apply =
  let position = sprintf "%d:%d" line col in
  [ "expand-prefix"; "-position"; position; "-prefix";  sprintf "\"%s\"" prefix; "-doc true -types true" ]
  |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.complete_prefix_answer_of_string json with
    | Return complete ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply complete.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let type_enclosing ~position:(line, col) ?expression ?cursor ?verbosity ?index ~filename ~source_code apply =
  let position = sprintf "%d:%d" line col in
  [
    ["type-enclosing"; "-position"; position ];
    (match expression with Some e -> ["-expression"; sprintf "\"%s\"" e ] | _ -> []);
    (match cursor with Some c -> ["-cursor"; string_of_int c ] | _ -> []);
    (match verbosity with Some v -> ["-verbosity"; v ] | _ -> []);
    (match index with Some i -> ["-index"; string_of_int i ] | _ -> [])
  ] |> List.concat
  |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.type_enclosing_answer_of_string json with
    | Return types ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply types.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let type_expression ~position:(line, col) ~expression ~filename ~source_code apply =
  let position = sprintf "%d:%d" line col in
  [ "type-expression"; "-position"; position; "-expression"; sprintf "\"%s\"" expression ]
  |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.type_expression_answer_of_string json with
    | Return type_expression ->
        Log.println `INFO "%s" (Yojson.Safe.prettify json);
        apply type_expression.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let document ~position:(line, col) ?identifier ~filename ~source_code apply =
  let position = sprintf "%d:%d" line col in
  "document" :: "-position" :: position ::
  (match identifier with None -> [] | Some identifier -> ["-identifier"; sprintf "\"%s\"" identifier])
  |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.document_answer_of_string json with
    | Return document ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply document.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let list_modules ?ext ~filename ~source_code apply =
  [ "list-modules"; "-ext .ml" ]
  |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.list_modules_answer_of_string json with
    | Return list_modules ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply list_modules.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

(*let locate (view : Ocaml_text.view) =
  let pos = view#buffer#get_iter_at_mark `INSERT in
  let position = sprintf "%d:%d" (pos#line + 1) pos#line_offset in
  [ "locate"; "-position"; position ]
  |> execute view

  let occurrences (view : Ocaml_text.view) =
  let pos = view#buffer#get_iter_at_mark `INSERT in
  let position = sprintf "%d:%d" (pos#line + 1) pos#line_offset in
  [ "occurrences "; "-identifier-at"; position ]
  |> execute view

  let complete_prefix (view : Ocaml_text.view) =
  let stop = view#buffer#get_iter_at_mark `INSERT in
  let position = sprintf "%d:%d" (stop#line + 1) stop#line_offset in
  let start = (stop#backward_find_char Glib.Unichar.isspace)#forward_char in
  let prefix = sprintf "'%s'" (view#buffer#get_text ~start ~stop ()) in
  [ "complete-prefix"; "-position"; position; "-prefix"; prefix ]
  |> execute view

  let outline (view : Ocaml_text.view) =
  let pos = view#buffer#get_iter_at_mark `INSERT in
  [ "outline " ]
  |> execute view ~continue_with:begin fun json ->
    (*json |> Yojson.Safe.prettify |> Printf.printf "%s\n%!";*)
    let outline = Merlin_j.answer_outline_of_string json in
    let rec print_items level items =
      items
      |> List.rev
      |> List.iter begin fun (item : Merlin_t.outline) ->
        let spcs = String.make (level * 2) ' ' in
        Printf.printf "%s%-6s %s : %s\n%!" spcs
          item.kind item.name (Option.value ~default:"" item.typ);
        item.children |> print_items (level + 1)
      end
    in
    outline.value |> print_items 0
  end
*)
