(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

  This file is part of OCamlEditor.

  OCamlEditor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  OCamlEditor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)


class manager ~view =
  let buffer = view#tbuffer in
  let [@inline] (=>) a b = not a || b in
  let match_whole_word_only = true in
  object (self)
    val mark_set = new mark_set ()
    val tag = buffer#create_tag ?name:(Some "mark_occurrences") []
    val mutable table : (GText.mark * GText.mark) list = []

    method table = table
    method tag : GText.tag = tag

    method clear () =
      if table <> [] then begin
        List.iter begin fun (m1, m2) ->
          let start = buffer#get_iter_at_mark m1 in
          let stop = buffer#get_iter_at_mark m2 in
          buffer#remove_tag tag ~start ~stop;
          buffer#delete_mark m1;
          buffer#delete_mark m2;
        end table;
        table <- [];
        mark_set#call()
      end

    method private get_word_at_iter ?iter () =
      buffer#select_word 
        ?iter 
        ?pat:(Some Ocaml_word_bound.regexp) 
        ?select:(Some false) 
        ?search:(Some true) () 

    method private get_text (start, stop) =
      buffer#get_text 
        ?start:(Some start) 
        ?stop:(Some stop) 
        ?slice:(Some false)
        ?visible:(Some false) ()

    method mark () =
      self#clear();
      match view#options#mark_occurrences with
      | true, under_cursor, _ ->
          let text = buffer#selection_text () in
          let text = 
            if text = "" then self#get_word_at_iter () |> self#get_text 
            else text 
          in
          let text = text |> String.trim in
          if String.length text > 1 && not (String.contains text '\n') && not (String.contains text '\r') then begin
            (*let vrect = view#visible_rect in
              let h0 = Gdk.Rectangle.height vrect in
              let y0 = Gdk.Rectangle.y vrect in
              let start, _ = view#get_line_at_y y0 in
              let stop, _ = view#get_line_at_y (y0 + h0) in
              let stop = stop#forward_line in*)
            let start = buffer#start_iter in
            let stop = buffer#end_iter in
            let iter = ref start in
            while !iter#compare stop < 0 do
              match !iter#forward_search ?flags:None ?limit:(Some stop) text with
              | Some (a, b) ->
                  let found = 
                    match_whole_word_only => (self#get_word_at_iter ~iter:a () |> self#get_text |> (=) text) 
                  in
                  if found then begin
                    buffer#apply_tag tag ~start:a ~stop:b;
                    let m1 = `MARK (buffer#create_mark ?name:None ?left_gravity:None a) in
                    let m2 = `MARK (buffer#create_mark ?name:None ?left_gravity:None b) in
                    table <- (m1, m2) :: table;
                  end;
                  iter := b;
              | _ -> iter := stop
            done;
            if table <> [] then mark_set#call()
          end
      | _ -> ()

    method connect = new signals ~mark_set
  end

and signals ~mark_set = object
  inherit GUtil.ml_signals [mark_set#disconnect]
  method mark_set = mark_set#connect ~after
end

and mark_set () = object inherit [unit] GUtil.signal () end

