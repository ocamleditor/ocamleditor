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


open Printf

type status = {
  mutable added    : int;
  mutable modified : int;
  mutable deleted  : int
}

let string_of_status status = match status with Some s -> sprintf " [+%d ~%d -%d]" s.added s.modified s.deleted | _ -> ""
let re_status = Str.regexp "\\([ MADRCU?!]\\)\\([ MDU?!]\\) +\\(.*\\)?"
let re_plus = Str.regexp "\\++"
let re_minus = Str.regexp "-+"

(** with_status *)
let with_status =
  let last_check = ref 0.0 in
  let last_status = ref None in
  fun ?(force=false) f ->
    match Oe_config.git_version with
      | None -> f None
      | _ ->
        let now = Unix.gettimeofday() in
        if force || now -. !last_check > 3.0 then begin
          last_check := now;
          let status = { added = 0; modified = 0; deleted = 0 } in
          let process_in =
            Spawn.loop begin fun ic ->
              let line = input_line ic in
              if Str.string_match re_status line 0 then begin
                let y = Str.matched_group 2 line in
                begin
                  match y with
                    | "M" -> status.modified <- status.modified + 1
                    | "?" -> status.added <- status.added + 1
                    | "D" -> status.deleted <- status.deleted + 1
                    | _ -> ()
                end;
              end;
            end
          in
          let has_errors = ref false in
          let process_err =
            Spawn.loop begin fun ic ->
              let _ = input_line ic in
              has_errors := true;
            end
          in
          Spawn.async ~process_in ~process_err
            ~at_exit:begin fun _ ->
              last_status := if !has_errors then None else Some status;
              if !has_errors then GtkThread.async f None
              else GtkThread.async f !last_status
            end "git" [|"status"; "--porcelain"|] |> ignore;
          last_check := now
        end else GtkThread.async f !last_status

(** with_diff_stat *)
let with_diff_stat f =
  let fact = -0.0 in
  let color_add = Color.add_value Oe_config.global_gutter_diff_color_add fact in
  let color_del = Color.add_value Oe_config.global_gutter_diff_color_del fact in
  match Oe_config.git_version with
    | None -> ()
    | _ ->
      let diffs = ref [] in
      let process_in =
        Spawn.loop begin fun ic ->
          let line = String.trim (input_line ic) in
          match Str.split (Miscellanea.regexp "\t") line with
            | [ins; del; fn] ->
              let ins = try int_of_string ins with Failure _ -> 0 in
              let del = try int_of_string del with Failure _ -> 0 in
              diffs := (ins, del, fn) :: !diffs;
            | _ -> ()
        end
      in
      let display diffs =
        let diffs = List.rev diffs in
        let ti, td, tc, mx = List.fold_left (fun (si, sd, sc, mx) (i, d, _) -> (si + i), (sd + d), (sc + i + d), (max mx (i+d))) (0, 0, 0, 0) diffs in
        let ftc = float tc in
        let count = List.length diffs in
        let tab = GPack.table ~border_width:8 ~row_spacings:3 ~col_spacings:8 ~columns:3 ~rows:count () in
        let w = min (float mx) 120. in
        let increment = if count <= 30 then 0 else if count <= 60 then -2 else -3 in
        List.iteri begin fun top (ins, del, fn) ->
          let cha = ins + del in
          let l1 = GMisc.label ~text:fn ~xalign:0.0 ~packing:(tab#attach ~top ~left:0) () in
          let l2 = GMisc.label ~text:(string_of_int cha) ~xalign:1.0 ~packing:(tab#attach ~top ~left:1) () in
          if count > 30 then begin
            let fd = Gtk_util.increase_font_size ~increment l1 in
            l1#misc#modify_font fd;
            l2#misc#modify_font fd;
          end;
          let ins = (float ins /. ftc) *. w in
          let del = (float del /. ftc) *. w in
          let ins = max 1 (int_of_float ins) in
          let del = max 1 (int_of_float del) in
          let hbox = GPack.hbox ~spacing:1 ~packing:(tab#attach ~top ~left:2) () in
          for _i = 1 to ins do
            let ebox = GBin.event_box ~width:8 ~height:8 ~packing:hbox#pack () in
            ebox#misc#modify_bg [`NORMAL, `NAME color_add];
          done;
          for _i = 1 to del do
            let ebox = GBin.event_box ~width:8 ~height:8 ~packing:hbox#pack () in
            ebox#misc#modify_bg [`NORMAL, `NAME color_del];
          done;
        end diffs;
        let markup = sprintf "\n%d files changed, <span weight='bold' color='%s'>%d</span> insertions, <span weight='bold' color='%s'>%d</span> deletions"
            count color_add ti color_del td in
        let l3 = GMisc.label ~markup ~xalign:0.0 ~packing:(tab#attach ~top:count ~left:0 ~right:3) () in
        if count > 30 then begin
          let fd = Gtk_util.increase_font_size ~increment l3 in
          l3#misc#modify_font fd;
        end;
        f tab#coerce
      in
      let has_errors = ref false in
      let process_err =
        Spawn.loop begin fun ic ->
          let _ = input_line ic in
          has_errors := true;
        end
      in
      Spawn.async ~process_in ~process_err
        ~at_exit:(fun _ -> if not !has_errors then GtkThread.async display !diffs)
        "git" [| "diff"; "--numstat"; "--color=never" |] |> ignore

let popup_window = ref None

let destroy popup =
  popup#destroy();
  popup_window := None;
  true

(** show_diff_stat *)
let show_diff_stat alloc widget =
  let popup = GWindow.window
    (*~kind:`POPUP ~type_hint:`MENU*) ?position:(match alloc with None -> Some `CENTER | _ -> None)
      ~decorated:false ~focus_on_map:true ~border_width:1 ~show:false () in
  popup_window := Some popup;
  popup#misc#set_can_focus true;
  popup#event#connect#focus_out ~callback:(fun _ -> destroy popup) |> ignore;
  popup#event#connect#key_press ~callback:begin fun ev ->
    if GdkEvent.Key.keyval ev = GdkKeysyms._Escape then destroy popup else false
  end |> ignore;
  let border_color = Color.add_value Preferences.preferences#get.Preferences.pref_bg_color_popup 0.1 in
  popup#misc#modify_bg [`NORMAL, `NAME border_color];
  let lbox = GBin.event_box ~packing:popup#add () in
  lbox#misc#modify_bg [`NORMAL, `NAME Preferences.preferences#get.Preferences.pref_bg_color_popup];
  lbox#add widget;
  begin
    match alloc with
      | Some alloc ->
        popup#move ~x:(-1000) ~y:(-1000);
        popup#show();
        popup#move ~x:(alloc.Gtk.x + alloc.Gtk.width - popup#misc#allocation.Gtk.width) ~y:(alloc.Gtk.y + alloc.Gtk.height + 8);
      | _ -> popup#show();
  end;
  let incr = if Preferences.preferences#get.Preferences.pref_annot_type_tooltips_delay = 0 then 0.106 else 0.479 in
  Gmisclib.Util.fade_window ~incr popup

(** install_popup *)
let install_popup (ebox : GBin.event_box) =
  ebox#event#connect#enter_notify ~callback:begin fun _ ->
    Gdk.Window.set_cursor ebox#misc#window (Gdk.Cursor.create `HAND1);
    false
  end |> ignore;
  ebox#event#connect#leave_notify ~callback:begin fun _ ->
    Gdk.Window.set_cursor ebox#misc#window (Gdk.Cursor.create `ARROW);
    false
  end |> ignore;
  ebox#event#connect#button_press ~callback:begin fun ev ->
    match !popup_window with
      | None when GdkEvent.Button.button ev = 1 ->
        with_diff_stat (show_diff_stat None (*(Some ebox#misc#allocation)*));
        true
      | _ -> true
  end |> ignore



