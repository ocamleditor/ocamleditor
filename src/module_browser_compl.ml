(*

  OCamlEditor
  Copyright (C) 2010, 2011 Francesco Tovagliari

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


open Module_browser
open Printf
open Code_insight
open GdkKeysyms

(** completion *)
class completion ~(page : Editor_page.page) ?packing () =
  let buffer          = page#buffer in
  let vbox            = GPack.vbox ~spacing:2 ~border_width:2 ?packing () in
  let tbox            = GPack.hbox ~spacing:5 ~border_width:0 ~packing:vbox#pack () in
  let ebox_resize     = GBin.event_box ~packing:(tbox#pack ~fill:false ~expand:false) () in
  let icon_grip       = GMisc.image ~pixbuf:Icons.grip ~packing:ebox_resize#add () in
  let ebox_title      = GBin.event_box ~packing:tbox#add () in
  let ebox_max        = GBin.event_box ~packing:tbox#pack () in
  let ebox_close      = GBin.event_box ~packing:tbox#pack () in
  let _               = List.iter Gmisclib.Util.set_ebox_invisible [ebox_title; ebox_max; ebox_close; ebox_resize] in
  let label_title     = GMisc.label ~markup:"" ~ypad:0 ~packing:ebox_title#add ~show:false () in
  let icon_full       = GMisc.image ~pixbuf:Icons.maximize ~packing:ebox_max#add () in
  let icon_close      = GMisc.image ~pixbuf:Icons.close ~packing:ebox_close#add () in
object (self)
  inherit GObj.widget vbox#as_widget
  val widget = new widget ~width:750 ~height:300 ~packing:vbox#add ()

  initializer Miscellanea.crono ~label:"init" self#init ()

  method private get_word () =
    let start, stop = page#buffer#as_text_buffer#select_word ~pat:Ocaml_word_bound.longid ~select:false () in
    buffer#get_text ~start ~stop:(buffer#get_iter `INSERT) ()

  method private set_title markup =
    kprintf label_title#set_label
      "<span color=\"%s\" weight=\"normal\" underline=\"none\" font_size=\"x-large\">%s</span>"
      !Preferences.preferences.Preferences.pref_fg_color_popup
      markup;
      label_title#misc#show()

  method search () =
    let text = self#get_word() in
    Printf.printf "------>%S\n%!" text;
    let insight = Miscellanea.crono ~label:"Code insight" (Code_insight.create ~text) page in
    match insight with
      | None -> ()
      | Some insight ->
        (** Title *)
          begin
          widget#vc_modlid#set_visible true;
          label_title#misc#hide();
          match insight.ci_mode with
            | Ci_module m ->
              widget#vc_modlid#set_visible false;
              kprintf self#set_title "Module  <tt><b>%s</b></tt>" insight.ci_title;
            | Ci_class (Some class_type) ->
              widget#vc_modlid#set_visible false;
              kprintf self#set_title "Class  <tt><b>%s</b></tt>" insight.ci_title;
            | _ -> ()
        end;
        widget#fill insight;
        (** Select row *)
        let pat = Str.regexp_string_case_fold insight.ci_text in
        widget#model#foreach begin fun path row ->
          let name = widget#model#get ~row ~column:widget#col_name in
          if Str.string_match pat name 0 then begin
            widget#view#set_cursor path widget#vc_descr;
            Gmisclib.Idle.add begin fun () ->
              try widget#view#scroll_to_cell ~align:(0.38, 0.0) path widget#vc_descr
              with Gpointer.Null -> ()
            end;
            true
          end else false
        end

  method init () =
    (** Colors *)
    widget#view#misc#modify_base [`NORMAL, `NAME !Preferences.preferences.Preferences.pref_bg_color_popup];
    widget#renderer#set_properties [`CELL_BACKGROUND !Preferences.preferences.Preferences.pref_bg_color_popup];
    widget#renderer_pixbuf#set_properties [`XPAD 3];
    let bg = !Preferences.preferences.Preferences.pref_bg_color_popup in
    widget#renderer_pixbuf#set_properties [`CELL_BACKGROUND bg];
    widget#vc_modlid#set_cell_data_func widget#renderer begin fun model row ->
      match (snd (model#get ~row ~column:widget#col_data)).ci_local with
        | false -> widget#renderer#set_properties [ `FOREGROUND_SET false ]
        | true -> widget#renderer#set_properties [ `FOREGROUND_SET true; `FOREGROUND "#0000ff" ]
    end;
    (** search and show window *)
    Miscellanea.crono ~label:"search" self#search ();
    let x, y = page#ocaml_view#get_location_at_cursor () in
    let window = Gtk_util.window self#coerce ~parent:page#ocaml_view ~x ~y () in
    (** key_press *)
    ignore (widget#connect#backspace ~callback:begin fun () ->
      let ins = buffer#get_iter `INSERT in
      if page#ocaml_view#editable then (buffer#delete ~start:ins ~stop:ins#backward_char);
      Gmisclib.Idle.add self#search;
    end);
    ignore (widget#view#event#connect#key_press ~callback:begin fun ev ->
      let state = GdkEvent.Key.state ev in
      let key = GdkEvent.Key.keyval ev in
      let ins = buffer#get_iter `INSERT in
      if key = _BackSpace then begin
        false
      end else if key = _Delete then begin
        if page#ocaml_view#editable then (buffer#delete ~start:ins ~stop:ins#forward_char);
        Gmisclib.Idle.add self#search;
        true
      end else if key = _Left then begin
        true;
      end else if key = _Right then begin
        true;
      end else if (state = [] || state = [`SHIFT]) && not (List.mem key [
          _Return;
          _Up;
          _Down;
          _Page_Up;
          _Page_Down;
          _End;
          _Home;
          _Escape
        ]) then begin
          if page#ocaml_view#editable then (buffer#insert ~iter:ins (GdkEvent.Key.string ev));
          Gmisclib.Idle.add self#search;
          true
      end else false;
    end);
    (** row_activated *)
    ignore (widget#view#connect#row_activated ~callback:begin fun path _ ->
      if page#ocaml_view#editable then begin
        match widget#view#selection#get_selected_rows with
          | [] -> ()
          | path :: _ ->
            let paths_opened = Lex.paths_opened (page#buffer#get_text ()) in
            let row = widget#model#get_iter path in
            let name = widget#model#get ~row ~column:widget#col_name in
            let modlid = widget#model#get ~row ~column:widget#col_modlid in
            let insight, data = widget#model#get ~row ~column:widget#col_data in
            let is_method = List.mem data.ci_kind [Pmethod; Pmethod_private; Pmethod_virtual; Pmethod_private_virtual] in
            let current_modlid = Miscellanea.modname_of_path page#get_filename  in
            let name =
              if is_method
              || List.mem modlid paths_opened
              || modlid = current_modlid
              || (match insight.ci_mode with Ci_module _ -> true | _ -> false)
              || modlid = ""
              then name else sprintf "%s.%s" modlid name
            in
            buffer#undo#begin_block ~name:"compl";
            buffer#select_word ~pat:Ocaml_word_bound.regexp ();
            buffer#delete_selection ();
            buffer#insert ~iter:(buffer#get_iter `INSERT) name;
            buffer#undo#end_block ();
            window#destroy()
      end
    end);
    (** Window buttons *)
    ignore (ebox_close#event#connect#button_press ~callback:(fun _ -> window#destroy(); true));
    ignore (ebox_max#event#connect#button_press ~callback:begin
      let is_fullscreen = ref false in fun _ ->
        if !is_fullscreen then (window#unmaximize()) else (window#maximize());
        is_fullscreen := not !is_fullscreen;
        true
    end);
    (** Window Move *)
    let motion = ref false in
    let xm0 = ref 0 in
    let ym0 = ref 0 in
    ignore (ebox_title#event#connect#button_press ~callback:begin fun ev ->
      xm0 := int_of_float (GdkEvent.Button.x ev);
      ym0 := int_of_float (GdkEvent.Button.y ev);
      Gdk.Window.set_cursor (GdkEvent.get_window ev) (Gdk.Cursor.create `FLEUR);
      motion := true;
      false
    end);
    ignore (ebox_title#event#connect#button_release ~callback:begin fun ev ->
      Gdk.Window.set_cursor (GdkEvent.get_window ev) (Gdk.Cursor.create `ARROW);
      motion := false;
      false
    end);
    ignore (ebox_title#event#connect#motion_notify ~callback:begin fun ev ->
      let x' = int_of_float (GdkEvent.Motion.x_root ev) in
      let y' = int_of_float (GdkEvent.Motion.y_root ev) in
      let x = x' - !xm0 - vbox#border_width - 21 in
      let y = y' - !ym0 - vbox#border_width in
      window#move ~x ~y;
      true
    end);
    (** Window Resize *)
    let resize = ref false in
    let xr0 = ref 0 in
    let yr0 = ref 0 in
    let width = ref 0 in
    let height = ref 0 in
    ignore (ebox_resize#event#connect#button_press ~callback:begin fun ev ->
      xr0 := int_of_float (GdkEvent.Button.x_root ev);
      yr0 := int_of_float (GdkEvent.Button.y_root ev);
      xm0 := int_of_float (GdkEvent.Button.x ev);
      ym0 := int_of_float (GdkEvent.Button.y ev);
      width := window#misc#allocation.Gtk.width;
      height := window#misc#allocation.Gtk.height;
      resize := true;
      false
    end);
    ignore (ebox_resize#event#connect#button_release ~callback:begin fun ev ->
      Gdk.Window.set_cursor (GdkEvent.get_window ev) (Gdk.Cursor.create `ARROW);
      resize := false;
      false
    end);
    ignore (ebox_resize#event#connect#enter_notify ~callback:begin fun ev ->
      Gdk.Window.set_cursor (GdkEvent.get_window ev) (Gdk.Cursor.create `TOP_LEFT_CORNER);
      false
    end);
    ignore (ebox_resize#event#connect#leave_notify ~callback:begin fun ev ->
      Gdk.Window.set_cursor (GdkEvent.get_window ev) (Gdk.Cursor.create `ARROW);
      false
    end);
    ignore (ebox_resize#event#connect#motion_notify ~callback:begin fun ev ->
      let x' = int_of_float (GdkEvent.Motion.x_root ev) in
      let y' = int_of_float (GdkEvent.Motion.y_root ev) in
      let dx = !xr0 - x' - vbox#border_width in
      let dy = !yr0 - y' - vbox#border_width in
      let w = (*max width0*) (!width + dx) in
      let h = (*max height0*) (!height + dy) in
      (*let px, py = Gdk.Window.get_position window#misc#window in
      let x = if dx > 0 then x' - !x0 - vbox#border_width else px in
      let y = if dy > 0 then y' - !y0 - vbox#border_width else py in*)
      let x = x' - !xm0 - vbox#border_width in
      let y = y' - !ym0 - vbox#border_width in
      window#resize ~width:w ~height:h;
      window#move ~x ~y;
      true
    end);

end


(** create *)
let create (page : Editor_page.page) = ignore (new completion ~page ());;


