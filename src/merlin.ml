open Printf

module Log = Common.Log.Make(struct let prefix = "MERLIN" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `ERROR

let loop (f : in_channel -> unit) ((ic, _oc, _ec) as channels) =
  try while true do f ic done
  with End_of_file ->
    Unix.close_process_full channels |> ignore

let execute
    ?(continue_with=fun x -> x |> Yojson.Safe.prettify |> Log.println `INFO "%s")
    filename source_code command =
  (*let cwd = Sys.getcwd() in
    let filename = match Utils.filename_relative cwd filename with Some path -> path | _ -> filename in*)
  let cmd_line = "ocamlmerlin" :: "server" :: command @ [ "-thread"; "-filename"; filename ] in
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
    (match verbosity with
     | Some `Smart -> ["-verbosity"; "smart" ]
     | Some (`Int n) -> ["-verbosity"; string_of_int n ]
     | _ -> []);
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

let list_modules ?(ext=[".ml"]) ?(filename="dummy") ?(source_code="") apply =
  "list-modules" :: (match ext with [] -> [] | _ -> ext |> List.map (sprintf "-ext %s"))
  |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.list_modules_answer_of_string json with
    | Return list_modules ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply list_modules.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let outline ~filename ~source_code apply =
  [ "outline" ] |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.outline_answer_of_string json with
    | Return outline ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply outline.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let locate ~position:(line, col) ?prefix ?look_for ~filename ~source_code apply =
  let position = sprintf "%d:%d" line col in
  "locate" :: "-position" :: position ::
  (match prefix with None -> "" | Some prefix -> sprintf "-prefix \"%s\"" prefix) ::
  (match look_for with
   | None -> ""
   | Some `Interface -> "-look-for interface"
   | Some `Implementation -> "-look-for implementation") :: []
  |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.locate_answer_of_string json with
    | Return document ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply document.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let locate_type ~position:(line, col) ~filename ~source_code apply =
  let position = sprintf "%d:%d" line col in
  "locate-type" :: "-position" :: position :: []
  |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.locate_type_answer_of_string json with
    | Return document ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply document.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let occurrences ~identifier_at:(line, col) ?scope ~filename ~source_code apply =
  let identifier_at = sprintf "%d:%d" line col in
  "occurrences" :: "-identifier-at" :: identifier_at ::
  (match scope with None -> "" | Some `Buffer -> sprintf "-scope buffer" | Some `Project -> "-scope project") :: []
  |> execute filename source_code ~continue_with:begin fun json ->
    match Merlin_j.occurrences_answer_of_string json with
    | Return document ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply document.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end
