type location = {
  filename : string;
  line : int;
  col : int;
}

let find ~filename ~buffer ~(iter : GText.iter) =
  Merlin.locate ~position:(iter#line + 1, iter#line_offset)
    ~filename
    ~buffer
    ~look_for:`Implementation ()
  |> Async.run_synchronously
  |> begin function
  | Merlin.Ok result ->
      begin
        match result with
        | `String msg ->
            Merlin.Error msg
        | `Assoc assoc ->
            let file, ln, col = ref None, ref 0, ref 0 in
            assoc |> List.iter (function
                | "file", `String x -> file := Some x
                | "pos", `Assoc ["line", `Int l; "col", `Int c]
                | "pos", `Assoc ["col", `Int c; "line", `Int l] -> ln := l - 1; col := c
                | _ -> ());
            let start = { Merlin_t.line = !ln; col = !col } in
            Merlin.Ok (Some { Merlin_t.file = !file; start; stop = start } )
        | _ ->
            Merlin.Ok None
      end
  | Merlin.Failure msg
  | Merlin.Error msg -> Merlin.Error msg
  end

let occurrences filename pos text =
  Merlin.occurrences ~identifier_at:pos
    ~filename
    ~scope:`Renaming
    ~buffer:text ()
  |> Async.start_with_continuation begin function
  | Merlin.Ok ranges ->
      let open Merlin_j in
      ranges
      |> List.iter begin fun range ->
        Printf.printf "%s %d:%d\n%!"
          (match range.file with Some x -> x | _ -> "LOCAL")
          range.start.line range.start.col;
      end
  | Merlin.Failure _ | Merlin.Error _ -> ()
  end

let rename editor =
  match editor#get_page `ACTIVE with
  | Some page ->
      let iter = page#buffer#get_iter `INSERT in
      let pos = iter#line + 1, iter#line_offset in
      let text = page#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None () in


      occurrences page#get_filename pos text;

      (*begin
        match [@warning "-4"] find ~filename:page#get_filename ~buffer:text ~iter with
        | Merlin.Ok (Some range) when range.Merlin_t.file = Some page#get_filename ->*)

      page#buffer#undo#begin_block ~name:"renaming";
      Printf.printf "---------------\n%!" ;
      page#view#mark_occurrences_manager#refs
      |> List.iter begin fun (m1, m2) ->
        let start = page#buffer#get_iter_at_mark m1 in
        let stop = page#buffer#get_iter_at_mark m2 in
        let pos = (start#offset + stop#offset) / 2 in
        let in_label = Lex.in_label text pos in

        let is_def =
          match [@warning "-4"] find ~filename:page#get_filename ~buffer:text ~iter:start with
          | Merlin.Ok (Some range) -> false
          | _ -> true
        in

        Printf.printf "--->%d:%d (%d)   %d:%d (%d)  %b %d --\n%!"
          start#line start#line_offset start#offset stop#line stop#line_offset stop#offset
          in_label pos
        (*range.start.line range.start.col (range.file = None)*);
        if not is_def && in_label then begin
          page#buffer#insert_interactive ?iter:(Some stop) ?default_editable:None ":test" |> ignore;
        end else begin
          page#buffer#delete_interactive ~start ~stop ?default_editable:None ();
          page#buffer#insert_interactive ?iter:(Some start) ?default_editable:None "test" |> ignore;
        end
      end;
      page#buffer#undo#end_block ();


      (*| _ -> ()
        end;*)
  | _ -> ()





