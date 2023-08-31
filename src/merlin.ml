open Printf
open Merlin_t

module Log = Common.Log.Make(struct let prefix = "MERLIN" end)
let _ = Log.set_verbosity `ERROR

let (//) = Filename.concat

let execute ?(continue_with=fun x -> x |> Yojson.Safe.prettify |> Printf.printf "%s\n%!") (view : Ocaml_text.view) command =
  let cmd_line =
    let filename =
      match (view :> Text.view)#tbuffer#file with
      | Some file -> file#filename
      | _ -> ""
    in
    let basename = Filename.basename filename in
    [ [ "ocamlmerlin"; "server" ];
      command;
      [ "-filename"; basename; "<"; basename ]
    ] |> List.concat
  in
  Printf.printf "%s\n%!" (cmd_line |> String.concat " ");
  Unix.open_process_in (cmd_line |> String.concat " ")
  |> Spawn.loop (fun ic -> ic |> input_line |> continue_with)

let check_configuration (view : Ocaml_text.view) =
  [ "check-configuration" ]
  |> execute view

(*let outline (view : Ocaml_text.view) =
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
  end*)

let enclosing (view : Ocaml_text.view) apply =
  let pos = view#buffer#get_iter_at_mark `INSERT in
  let position = sprintf "%d:%d" (pos#line + 1) pos#line_offset in
  [ "enclosing"; "-position"; position ]
  |> execute view ~continue_with:begin fun json ->
    match Merlin_j.enclosing_answer_of_string json with
    | Return enclosing ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply enclosing.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let case_analysis (view : Ocaml_text.view) apply =
  let start, stop = view#buffer#selection_bounds in
  let start = sprintf "%d:%d" (start#line + 1) start#line_offset in
  let stop = sprintf "%d:%d" (stop#line + 1) stop#line_offset in
  [ "case-analysis"; "-start"; start; "-end"; stop ]
  |> execute view ~continue_with:begin fun json ->
    match Merlin_j.case_analysis_answer_of_string json with
    | Return case_analysis ->
        Log.println `DEBUG "%s" (Yojson.Safe.prettify json);
        apply case_analysis.value
    | Failure msg
    | Error msg
    | Exception msg -> Log.println `ERROR "%s" msg.value;
  end

let locate (view : Ocaml_text.view) =
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
