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
open Cairo_drawable

module Diff = struct

  type 'a editor_page =
    < buffer : < line_count : int; orig_filename : string;
                 save_buffer : ?filename:string -> unit -> string * (string * string) option;
                 tmp_filename : string; .. >;
      global_gutter : GMisc.drawing_area;
      global_gutter_tooltips : ((int * int * int * int) * (unit -> GObj.widget)) list;
      changed_after_last_diff : bool;
      set_changed_after_last_diff : bool -> unit;
      set_global_gutter_tooltips : ((int * int * int * int) * (unit -> GObj.widget)) list -> unit;
      view : < gutter : Gutter.t; ..>;
      misc : < allocation : Gtk.rectangle; ..>;
      get_oid : int;
      .. > as 'a

  let global_gutter_diff_size = 8
  let global_gutter_diff_sep = 1
  let fact = 0.0
  let color_add = `NAME (Color.add_value Oe_config.global_gutter_diff_color_add fact)
  let color_del = `NAME (Color.add_value Oe_config.global_gutter_diff_color_del fact)
  let color_change = `NAME (Color.add_value Oe_config.global_gutter_diff_color_change 0.2)

  let initialized : int list ref = ref []

  (** init *)
  let init page =
    if not (List.mem page#get_oid !initialized) then begin
      let change_size () =
        let height : int = page#global_gutter#height_request in
        let new_width : int = Oe_config.global_gutter_size + global_gutter_diff_size + global_gutter_diff_sep in
        page#global_gutter#set_height_request height;
        page#global_gutter#set_width_request new_width;
        page#global_gutter#event#connect#button_release ~callback:begin fun ev ->
          if (GdkEvent.Button.button ev = 3 && GdkEvent.get_type ev = `BUTTON_RELEASE) then begin
            let x = GdkEvent.Button.x ev in
            let y = GdkEvent.Button.y ev in
            if int_of_float x < global_gutter_diff_size then begin
              (*Printf.printf "----> %f, %f\n%!" x y;*)
            end;
          end;
          true
        end |> ignore;
        initialized := page#get_oid :: !initialized;
      in
      try change_size () with Gpointer.Null ->
        page#global_gutter#misc#connect#after#realize ~callback:change_size |> ignore
    end

  (** create_label_tooltip *)
  let create_label_tooltip elements =
    let ebox = GBin.event_box () in
    let vbox = GPack.vbox ~spacing:0 ~packing:ebox#add () in
    let color = fst Preferences.preferences#get.Preferences.pref_bg_color in
    ebox#misc#modify_bg [`NORMAL, `NAME color];
    let font_description = GPango.font_description_from_string Preferences.preferences#get.Preferences.pref_base_font in
    let size = font_description#size - 1 * Pango.scale in
    font_description#modify ~size ();
    let last = List.length elements - 1 in
    List.iteri begin fun i (color, markup) ->
      let hbox = GPack.hbox ~spacing:2 ~packing:vbox#pack () in
      let ebox = GBin.event_box ~width:13 ~packing:hbox#pack () in
      ebox#misc#modify_bg [`NORMAL, color];
      let label = GMisc.label ~xalign:0.0 ~yalign:0.5 ~markup ~packing:hbox#add () in
      label#misc#modify_font font_description;
      if i < last then GMisc.separator `HORIZONTAL ~packing:vbox#add () |> ignore
    end elements;
    ebox#coerce

  (** create_tooltip *)
  let create_tooltip page width y h text =
    if Oe_config.global_gutter_diff_tooltips then begin
      let create_markup = Lexical_markup.parse Preferences.preferences#get in
      let create_markup text = create_markup text in
      let tooltip_func () =
        match text with
        | [color, text] -> create_label_tooltip [color, create_markup text]
        | [_, text1; _, text2] ->
          create_label_tooltip [
            color_del, create_markup text1;
            color_add, create_markup text2;
          ];
        | _ -> create_label_tooltip []
      in
      page#set_global_gutter_tooltips (((0, y - 2, width, h + 2), tooltip_func) :: page#global_gutter_tooltips)
    end

  (** wave_line *)
  let wave_line ~(drawable : Gdk.cairo) ~color ~width ~height ~y ~is_add =
    set_foreground drawable color;
    let ww = width * 2 / 3 in
    let len = 0 (*height / ww*) in
    if len < 4 then
      rectangle drawable ~filled:is_add ~x:0 ~y ~width:(width - (if is_add then 1 else 2)) ~height ()
    else
      let polyline = ref [] in
      let i = ref 0 in
      while !i < len - 1 do
        polyline := (ww, y + ww * (!i + 1)) :: (0, y + ww * !i) :: !polyline;
        incr i;
        incr i;
      done;
      if !polyline <> [] then begin
        let width = if is_add then 1 else 2 in
        set_line_attributes drawable ~width ~cap:`ROUND ~style:`SOLID ~join:`ROUND ();
        lines drawable !polyline
      end;;

  let cache : (int, Odiff.diffs) Hashtbl.t = Hashtbl.create 17

  (** paint_diffs *)
  let paint_diffs page diffs =
    let window = page#global_gutter#misc#window in
    let drawable = GDraw.Cairo.create window in
    let alloc = page#misc#allocation in
    set_line_attributes drawable ~width:1 ~style:`SOLID ~join:`ROUND ();
    let height = alloc.Gtk.height in
    let width = global_gutter_diff_size in
    let line_count = float page#buffer#line_count in
    let open Odiff in
    set_foreground drawable page#view#gutter.Gutter.bg_color;
    rectangle drawable ~filled:true ~x:0 ~y:0 ~width ~height ();
    page#set_global_gutter_tooltips [];
    let black = page#view#gutter.Gutter.marker_color in
    let height = height - 2 * alloc.Gtk.width in
    let height = float height in
    let y_of_line ln = alloc.Gtk.width + int_of_float ((float ln /. line_count) *. height) in
    let line_height = int_of_float (height /. line_count) in
    let wtri = width * 2 / 3 in
    let paint color elems = function
      | One ln ->
        let x0 = 0 (*width - wtri*) in
        let y = y_of_line ln in
        let height = max 3 line_height in
        begin
          match color with
          | col when col = color_del ->
            begin
              match Oe_config.global_gutter_diff_style with
              | `COLOR with_border ->
                set_foreground drawable color;
                polygon drawable ~filled:true [x0, y; x0 + wtri, y - wtri; x0 + wtri, y + wtri];
                if (*true ||*) with_border then begin
                  set_foreground drawable (Color.set_value 0.6 color);
                  polygon drawable ~filled:false [0, y; wtri, y - wtri; wtri, y + wtri]
                end
              | `BW ->
                set_foreground drawable black;
                let tri = [x0, y; x0 + wtri, y - wtri; x0 + wtri, y + wtri] in
                polygon drawable ~filled:true tri;
                set_foreground drawable (Color.set_value 0.5 black);
                polygon drawable ~filled:false tri;
            end;
          | col when col = color_add ->
            set_foreground drawable
              (match Oe_config.global_gutter_diff_style with
               | `COLOR _ -> color;
               | `BW -> black);
            rectangle drawable ~filled:true ~x:0 ~y:(y - 2) ~width:(width-2) ~height ();
          | color ->
            begin
              match Oe_config.global_gutter_diff_style with
              | `COLOR with_border ->
                set_foreground drawable color;
                rectangle drawable ~filled:true ~x:0 ~y:(y - 2) ~width ~height ();
                if with_border then begin
                  set_foreground drawable (Color.set_value 0.6 color);
                  rectangle drawable ~filled:false ~x:0 ~y:(y - 2) ~width:(width-1) ~height ();
                end
              | `BW ->
                set_foreground drawable black;
                rectangle drawable ~filled:false ~x:0 ~y:(y - 2) ~width:(width - 1) ~height ();
            end;
        end;
        create_tooltip page width y height elems
      | Many (l1, l2) ->
        let y1 = y_of_line l1 in
        let y2 = y_of_line l2 in
        let height = max 3 (y2 - y1 + line_height) in
        begin
          match Oe_config.global_gutter_diff_style with
          | `COLOR with_border ->
            set_foreground drawable color;
            rectangle drawable ~filled:true ~x:0 ~y:y1 ~width ~height ();
            if with_border then begin
              set_foreground drawable (Color.set_value 0.6 color);
              rectangle drawable ~filled:false ~x:0 ~y:y1 ~width:(width-1) ~height ();
            end;
          | `BW ->
            wave_line ~drawable ~color:black ~width ~height ~y:y1 ~is_add:(color = color_add);
        end;
        create_tooltip page width y1 height elems
    in
    let diffs = List.sort begin fun a b ->
        match a with
        | Delete _ -> (match b with Delete _ -> 0 | _ -> 1)
        | _ -> (match b with Delete _ -> -1 | _ -> 0)
      end diffs
    in
    List.iter begin fun diff ->
      Gmisclib.Idle.add ~prio:200 begin fun () ->
        match diff with
        | Add (_, ind, a) ->
          paint color_add [color_add, a] ind |> ignore;
        | Delete (_, ind, a) ->
          paint color_del [color_del, a] ind |> ignore;
        | Change (_, a, ind, b) ->
          paint color_change [color_del, a; color_add, b] ind
      end
    end diffs


  (** paint_gutter *)
  let rec paint_gutter page =
    if page#changed_after_last_diff then begin
      let buffer = page#buffer in
      let filename2 = buffer#tmp_filename in
      buffer#save_buffer ?filename:None () |> ignore;
      let diff = Preferences.preferences#get.Preferences.pref_program_diff in
      let args = [|
        "--binary";
        buffer#orig_filename;
        filename2
      |]
      in
      page#set_changed_after_last_diff false;
      let diffs = ref [] in
      let process_in ic =
        try diffs := Odiff.from_channel ic
        with ex -> Printf.eprintf "File \"plugin_diff.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
      in
      Spawn.async diff args
        ~process_in (*~process_err:Spawn.redirect_to_stderr*)
        ~at_exit:begin fun _ ->
          Hashtbl.replace cache page#get_oid !diffs;
          match !diffs with [] -> () | diffs ->
            (*Printf.printf "%s\n%!" (Odiff.string_of_diffs diffs);*)
            paint_diffs page diffs
        end |> ignore
    end else begin
      try
        let diffs = Hashtbl.find cache page#get_oid in
        paint_diffs page diffs
      with Not_found ->
        page#set_changed_after_last_diff true;
        paint_gutter page;
    end


  (** to_buffer *)
  let to_buffer (buffer : GText.buffer) filename1 filename2 =
    Plugin_diff_gtext.insert buffer filename1 filename2

end

(** Initialization *)
let _ =
  begin
    match !Browser.browser with
    | Some browser ->
      let editor = browser#editor in
      (* Init editor pages *)
      browser#editor#with_current_page Diff.init;
      let callback page = Gmisclib.Idle.add ~prio:100 (fun () -> Diff.init page) in
      editor#connect#add_page ~callback |> ignore;
      editor#connect#switch_page ~callback |> ignore;
      editor#connect#switch_page ~callback:Diff.paint_gutter |> ignore;
      editor#connect#remove_page ~callback:begin fun page ->
        Diff.initialized := List.filter ((<>) page#get_oid) Diff.(!initialized);
        Hashtbl.remove Diff.cache page#get_oid;
      end |> ignore;
      (* Timeout *)
      let id_timeout_diff = ref None in
      let create_timeout_diff () =
        Gaux.may !id_timeout_diff ~f:GMain.Timeout.remove;
        id_timeout_diff := None;
        let callback () =
          try
            editor#with_current_page begin fun page ->
              if page#view#has_focus then (Diff.paint_gutter page);
            end;
            true
          with ex ->
            Printf.eprintf "File \"plugin_diff.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
            true
        in
        editor#with_current_page Diff.paint_gutter;
        id_timeout_diff := Some (GMain.Timeout.add ~ms:3000 ~callback);
      in
      let bind _ =
        create_timeout_diff();
        Gaux.may (GWindow.toplevel editor#coerce) ~f:begin fun (w : GWindow.window) ->
          w#event#connect#focus_in ~callback:begin fun _ ->
            create_timeout_diff();
            false
          end |> ignore;
          w#event#connect#focus_out ~callback:begin fun _ ->
            Gaux.may !id_timeout_diff ~f:GMain.Timeout.remove;
            id_timeout_diff := None;
            false
          end |> ignore;
        end;
      in
      begin
        try bind ()
        with Gpointer.Null -> editor#misc#connect#map ~callback:bind |> ignore;
      end;
    | _ -> failwith "Cannot initialize Plugin_diff because the browser object has not yet been created."
  end;


  Plugins.diff := Some (module Diff : Plugins.DIFF)


