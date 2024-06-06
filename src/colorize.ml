open Utils

let idleize ?prio f c () =
  Gmisclib.Idle.add ?prio (f (); c);;

let cascade_rev ff =
  List.fold_left (fun acc f -> f acc) ignore ff;;

let idleize_cascade ?prio x =
  x |> List.map (idleize ?prio) |> cascade_rev;;

let colorize_buffer (view : Ocaml_text.view) =
  let buffer = view#obuffer in
  if buffer#lexical_enabled then begin
    buffer#init_tags ();
    let buffer = (buffer :> GText.buffer) in
    let colorize start stop () = Lexical.tag ~start ~stop buffer in
    let vstart, vstop, vlines =
      let vrect = view#visible_rect in
      let h0 = Gdk.Rectangle.height vrect in
      let y0 = Gdk.Rectangle.y vrect in
      let start, _ = view#get_line_at_y y0 in
      let stop, _ = view#get_line_at_y (y0 + h0) in
      let stop = stop#forward_line in
      start, stop, stop#line - start#line
    in
    colorize vstart vstop ();
    let steps = ref [] in
    let make_steps start stop =
      let iter = ref start in
      while !iter#compare stop < 0 do
        let start = !iter#copy in
        let stop = !iter#forward_lines vlines in
        steps := (colorize start stop) :: !steps;
        iter := stop;
      done;
    in
    make_steps buffer#start_iter vstart;
    make_steps vstop buffer#end_iter;
    idleize_cascade ~prio:300 !steps ()
  end;

