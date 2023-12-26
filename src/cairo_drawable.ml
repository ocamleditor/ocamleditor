
(* Cairo helpers *)
let f = float_of_int;;

let line drawable x1 y1 x2 y2 =
  let open Cairo in
  move_to drawable (f x1) (f y1);
  line_to drawable (f x2) (f y2);
  stroke drawable
;;

let lines drawable (x1, y1) xys =
  let open Cairo in
  move_to drawable (f x1) (f y1);
  List.iter (fun (x, y) ->
      line_to drawable (f x) (f y);
      stroke drawable
    ) xys

let lines drawable xys =
  lines drawable (List.hd xys) (List.tl xys)

let rec lines_ drawable = function
  | [] -> ()
  | (x1, y1) :: (x2, y2) :: more -> line drawable x1 y1 x2 y2; lines_ drawable more
  | _ -> assert false

let rec segments drawable = function
  | [] -> ()
  | ((x1, y1), (x2, y2)) :: more ->
    line drawable x1 y1 x2 y2; segments drawable more

let rectangle drawable ~x ~y ~width ~height ?(filled = false) () =
  Cairo.move_to drawable (f x) (f y);
  Cairo.line_to drawable (f (x + width)) (f y);
  Cairo.line_to drawable (f (x + width)) (f (y + height));
  Cairo.line_to drawable (f x) (f (y + height));
  Cairo.line_to drawable (f x) (f y);

  if filled then Cairo.fill drawable else Cairo.stroke drawable
;;

let set_foreground drawable color =
  let color = GDraw.color color in
  let r = (Gdk.Color.red color |> f) /. 65535.0 in
  let g = (Gdk.Color.green color |> f) /. 65535.0 in
  let b = (Gdk.Color.blue color |> f) /. 65535.0 in
  Cairo.set_source_rgb drawable r g b
;;

let set_line_attributes drawable ~width ?(style = `SOLID) ?(join = `BEVEL) ?(cap = `BUTT) () =
  Cairo.set_line_width drawable (f width);
  ( match style with
    | `ON_OFF_DASH -> Cairo.set_dash drawable [| 3.0 |]
    | `DOUBLE_DASH -> Cairo.set_dash drawable [| 3.0; 1.0 |]
    | `SOLID       -> Cairo.set_dash drawable [|     |]
  );
  ( match join with
    | `ROUND -> Cairo.set_line_join drawable Cairo.JOIN_ROUND
    | `MITER -> Cairo.set_line_join drawable Cairo.JOIN_MITER
    | `BEVEL -> Cairo.set_line_join drawable Cairo.JOIN_BEVEL
  );
  ( match cap with
    | `PROJECTING
    | `BUTT -> Cairo.set_line_cap drawable Cairo.BUTT
    | `ROUND -> Cairo.set_line_cap drawable Cairo.ROUND
    | `SQUARE -> Cairo.set_line_cap drawable Cairo.SQUARE
  )
;;

let path drawable pt more =
  let open Cairo in
  let rec lines = function
    | [] -> ()
    | (x, y) :: rest -> line_to drawable (f x) (f y); lines rest
  in
  let x, y = pt in
  move_to drawable (f x) (f y);
  lines more;
  line_to drawable (f x) (f y)

let polygon drawable ?(filled = false) points =
  match points with
  | [] -> assert false
  | pt :: more -> path drawable pt more;

    if filled then Cairo.fill drawable else Cairo.stroke drawable
