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
