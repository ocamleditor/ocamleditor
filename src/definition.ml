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
            let file, ln, col = ref "", ref 0, ref 0 in
            assoc |> List.iter (function
                | "file", `String x -> file := x
                | "pos", `Assoc ["line", `Int l; "col", `Int c]
                | "pos", `Assoc ["col", `Int c; "line", `Int l] -> ln := l - 1; col := c
                | _ -> ());
            Merlin.Ok (Some { filename = !file; line = !ln (*+ 1*); col = !col (*+ 1*) } )
        | _ ->
            Merlin.Ok None
      end
  | Merlin.Failure msg
  | Merlin.Error msg -> Merlin.Error msg
  end
