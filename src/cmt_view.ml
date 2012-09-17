(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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
open GUtil
open Miscellanea
open Cmt_format
open Location
open Typedtree

let cols              = new GTree.column_list
let col_icon          = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf")
let col_markup        = cols#add Gobject.Data.string
(*let col_name          = cols#add Gobject.Data.string
let col_name_sort     = cols#add Gobject.Data.string
let col_type          = cols#add Gobject.Data.string
let col_id            = cols#add Gobject.Data.int*)

let string_of_loc loc =
  let filename, a, b = Location.get_pos_info loc.loc_start in
  let _, c, d = Location.get_pos_info loc.loc_end in
  sprintf "%s, %d:%d -- %d:%d" filename a b c d

class widget ~editor ~page ?packing () =
  let sw              = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ?packing () in
  let model           = GTree.tree_store cols in
  let view            = GTree.view ~model ~headers_visible:false ~packing:sw#add ~width:150 ~height:300 () in
  let renderer_pixbuf = GTree.cell_renderer_pixbuf [`YPAD 0; `XPAD 0] in
  let renderer_markup = GTree.cell_renderer_text [`YPAD 0] in
  let vc              = GTree.view_column () in
  let _               = vc#pack ~expand:false renderer_pixbuf in
  let _               = vc#pack ~expand:false renderer_markup in
  let _               = vc#add_attribute renderer_pixbuf "pixbuf" col_icon in
  let _               = vc#add_attribute renderer_markup "markup" col_markup in
  let _               = view#selection#set_mode `SINGLE in
  let _               = view#append_column vc in
  let _               = view#misc#set_property "enable-tree-lines" (`BOOL true) in
  let _               = view#misc#modify_font_by_name Preferences.preferences#get.Preferences.pref_compl_font in
  let _               = view#misc#modify_base [`SELECTED, `NAME Oe_config.outline_selection_bg_color; `ACTIVE, `NAME Oe_config.outline_active_bg_color] in
  let _               = view#misc#modify_text [`SELECTED, `NAME Oe_config.outline_selection_fg_color; `ACTIVE, `NAME Oe_config.outline_active_fg_color] in
object (self)
  inherit GObj.widget sw#as_widget
  val changed = new changed()
  initializer
    let source = page#get_filename in
    match Project.tmp_of_abs editor#project source with
      | Some (tmp, relname) ->
        self#load (tmp // relname);
      | _ -> ()

  method load filename =
    let filename_cmt = (Filename.chop_extension filename) ^ ".cmt" in
    let cmt = Cmt_format.read_cmt filename_cmt in
    model#clear();
    self#parse cmt.cmt_annots

  method private parse = function
    | Implementation impl -> self#parse_struct impl.str_items
    | Partial_implementation part_impl ->
      let row = model#append () in
      model#set ~row ~column:col_icon Icons.attribute;
      model#set ~row ~column:col_markup "Partial_implementation"
    | Interface intf -> ()
    | Partial_interface part_intf -> ()
    | Packed _ -> ()

  method private append ?parent ?icon markup =
    let row = model#append ?parent () in
    Gaux.may icon ~f:(model#set ~row ~column:col_icon);
    model#set ~row ~column:col_markup markup;
    row

  method private parse_struct =
    List.iter begin fun item ->
      match item.str_desc with
        | Tstr_eval expr -> ignore (self#append "Tstr_eval")
        | Tstr_value (_, pe) ->
          List.iter (fun (pat, expr) -> ignore (self#append  (self#parse_pattern pat.pat_desc))) pe
        | Tstr_exception (id, loc, _) -> ignore (self#append (Ident.name id))
        | Typedtree.Tstr_class classes -> List.iter self#parse_class_decl classes
        | _ -> ()
    end

  method private parse_pattern = function
    | Tpat_var (id, _) -> Ident.name id
    | _ -> ""

  method private parse_class_expr_desc ~parent = function
    | Tcl_structure str ->
      List.map begin fun fi ->
        match fi.cf_desc with
          | Tcf_inher (_, cl_expr, id, inherited_fields, inherited_methods) ->
            let parent = self#parse_class_expr_desc ~parent cl_expr.cl_desc in
            List.iter (fun (x, _) -> ignore (self#append ~parent (sprintf "<i>%s</i>" x))) inherited_methods;
            parent
          | Tcf_init expr ->
            self#append ~parent (sprintf "<i>initializer</i>:%s" (string_of_loc fi.cf_loc));
          | _ -> self#append ~parent (sprintf "Tcf_...");
      end str.cstr_fields;
      parent
    | Tcl_fun (_, _, _, cl_expr, _) ->
      self#parse_class_expr_desc ~parent cl_expr.cl_desc;
    | Tcl_ident (_, lid, _) ->
      self#append ~parent ~icon:Icons.class_inherit (String.concat "." (Longident.flatten lid.txt));
    | Tcl_apply (cl_expr, _) ->
      self#parse_class_expr_desc ~parent cl_expr.cl_desc;
    | Tcl_let (_, _, _, desc) ->
      self#parse_class_expr_desc ~parent desc.cl_desc;
    | Tcl_constraint (cl_expr, _, _, _, _) ->
      self#parse_class_expr_desc ~parent cl_expr.cl_desc;

  method private parse_class_decl (infos, _, _) =
    let icon = if infos.ci_virt = Asttypes.Virtual then Icons.class_virtual else Icons.classe in
    let parent = self#append ~icon infos.ci_id_name.txt in
    ignore (self#parse_class_expr_desc ~parent infos.ci_expr.cl_desc);

  method connect = new signals ~changed
end

and changed () = object inherit [string * bool] signal () end
and signals ~changed =
object
  inherit ml_signals [changed#disconnect]
  method changed = changed#connect ~after
end

let create = new widget

let window ~editor ~page () =
  let window = GWindow.window ~position:`CENTER ~show:false () in
  let vbox = GPack.vbox ~packing:window#add () in
  let widget = create ~editor ~page ~packing:vbox#add () in
  ignore (window#event#connect#key_press ~callback:begin fun ev ->
    if GdkEvent.Key.keyval ev = GdkKeysyms._Escape then (window#destroy (); true)
    else false
  end);
  window#present();
  widget, window;;
