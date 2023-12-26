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
[@@@warning "-48"]

(** A widget used to choose from a list of items, like GtkComboBox,
    which allows you to select an item by searching as you type. *)
class widget ?model ?width ?(popup_width=280) ?(popup_height=300) ?packing () =
  let model =
    match model with Some x -> x | _ ->
      let cols = new GTree.column_list in
      let _ = cols#add Gobject.Data.string in
      GTree.list_store cols
  in
  let hbox    = GPack.hbox ~spacing:0 ?packing () in
  let button  = GButton.toggle_button ~active:false ~packing:hbox#add (*(hbox#pack ~fill:false ~expand:false)*) () in
  let box     = GPack.hbox ~spacing:3 ~border_width:1 ~packing:button#add () in
  let label   = GMisc.label ~markup:"" ?width ~xalign:0.0 ~yalign:0.0 ~ellipsize:`END ~packing:box#add () in
  let _       = GMisc.separator `VERTICAL ~packing:box#pack () in
  let _       = GMisc.arrow ~height:7 ~xpad:1 ~ypad:0 ~xalign:0.5 ~yalign:0.5 ~shadow:`NONE ~kind:`DOWN ~packing:box#pack () in
  let _       = button#misc#set_can_focus true in
  let _       = button#misc#set_can_default false in
  let _       = button#set_focus_on_click false in
  let popup   = new Window.popup ~widget:button#coerce () in
  let sw      = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:popup#add () in
  let view    = GTree.view ~model ~packing:sw#add () in
  let _       =
    view#set_headers_visible false;
    view#selection#select_path (GTree.Path.create [0]);
  in
  let changed = new changed () in
object (self)
  inherit GObj.widget hbox#as_widget

  val mutable markup_column : string GTree.column option = None
  val mutable label_column : string GTree.column option = None
  val mutable value_column : string option GTree.column option = None
  val mutable default_column : bool GTree.column option = None
  val mutable vc_markup = None
  val mutable sign_id = None
  val active_value : string option GUtil.variable = new GUtil.variable None

  method set_markup_column col =
    markup_column <- Some col;
    let renderer_markup = GTree.cell_renderer_text [] in
    let vc = GTree.view_column ~renderer:(renderer_markup, ["markup", col]) () in
    ignore (view#append_column vc);
    vc_markup <- Some vc;

  method set_value_column col = value_column <- Some col
  method set_label_column col = label_column <- Some col
  method set_default_column col = default_column <- Some col

  method private get_label ~iter:row =
    match label_column with
      | Some column -> model#get ~row ~column
      | _ -> (match self#get_value ~iter:row with Some x -> x | _ -> "")

  method private get_markup ~iter:row =
    match markup_column with
      | Some column -> model#get ~row ~column
      | _ -> ""

  method private get_value ~iter:row =
    match value_column with
      | Some column -> model#get ~row ~column
      | _ -> invalid_arg "entry_combo_search.ml (get_value)"

  method active = active_value#get

  method set_active = function
    | Some _ as value ->
      active_value#set value;
      let found = ref false in
      model#foreach begin fun path iter ->
        let v = self#get_value ~iter in
        if v = value then begin
          Gaux.may vc_markup ~f:(view#set_cursor path);
          self#set_label iter;
          found := true;
          true
        end else false
      end;
      !found;
    | None ->
      begin
        let select_none () =
          active_value#set None;
          label#set_label "";
          view#selection#unselect_all();
        in
        match default_column with
          | Some column ->
            let found = ref false in
            model#foreach begin fun path row ->
              let is_default_value = model#get ~row ~column in
              if is_default_value then begin
                Gaux.may value_column ~f:(fun column -> active_value#set (model#get ~row ~column));
                self#set_label row;
                Gaux.may vc_markup ~f:(view#set_cursor path);
                found := true;
                true
              end else false
            end;
            if not !found then (select_none())
          | _ -> select_none()
      end;
      true

  method private set_label iter =
    label#set_label (self#get_label ~iter);
    button#misc#set_tooltip_markup (self#get_markup ~iter);

  initializer
    Gaux.may (GWindow.toplevel self#coerce) ~f:(fun top -> popup#set_transient_for top#as_window);
    Gaux.may (GWindow.toplevel self#coerce) ~f:(fun top -> popup#set_modal top#modal);
    ignore (popup#event#connect#key_press ~callback:begin fun ev ->
      if GdkEvent.Key.keyval ev = GdkKeysyms._Escape then begin
        popup#popdown();
        true
      end
      else false
    end);
    (* Toggled *)
    sign_id <- Some (button#connect#toggled ~callback:begin fun () ->
      if popup#misc#visible then (popup#popdown()) else begin
          assert (self#set_active active_value#get);
          popup#present();
          button#misc#set_tooltip_markup "";
          let alloc = label#misc#allocation in
          Gmisclib_util.idle_add (fun () -> popup#resize ~width:(max popup_width alloc.Gtk.width) ~height:popup_height);
          Gmisclib_util.idle_add button#misc#grab_focus;
      end
    end);
    (* on_popdown *)
    let on_popdown () =
      Gaux.may sign_id ~f:button#misc#handler_block;
      button#set_active false;
      Gaux.may sign_id ~f:button#misc#handler_unblock;
      match view#selection#get_selected_rows with
        | path :: _ ->
          let row = model#get_iter path in
          self#set_label row;
        | [] -> ()
    in
    popup#set_on_popdown on_popdown;
    (* set_hover_selection *)
    view#set_hover_selection true;
    ignore (view#event#connect#button_release ~callback:begin fun _ ->
      match view#selection#get_selected_rows with
        | path :: _ ->
          self#activate path;
          true
        | [] -> false
    end);
    (*  *)
    ignore (view#connect#row_activated ~callback:(fun path _ -> self#activate path));
    ignore (active_value#connect#changed ~callback:changed#call);

  method private activate path =
    let iter = model#get_iter path in
    let value = self#get_value ~iter in
    popup#popdown();
    assert (self#set_active value);
    self#set_label iter;
    Gmisclib_util.idle_add button#misc#grab_focus;

  method connect = new signals ~changed
end

and changed () = object inherit [string option] GUtil.signal () end
and signals ~changed =
  object
    inherit GUtil.ml_signals [changed#disconnect]
    method changed = changed#connect ~after
  end
