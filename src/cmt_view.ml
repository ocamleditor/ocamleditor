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
open Asttypes

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
  let view            = GTree.view ~model ~headers_visible:false ~packing:sw#add ~width:350 ~height:500 () in
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
    self#parse cmt.cmt_annots;

  method private parse = function
    | Implementation impl -> List.iter self#append_struct impl.str_items
    | Partial_implementation part_impl ->
      let row = model#append () in
      model#set ~row ~column:col_icon Icons.attribute;
      model#set ~row ~column:col_markup "Partial_implementation"
    | Interface intf -> ()
    | Partial_interface part_intf -> ()
    | Packed _ -> ()

  method private append ?parent ?icon markup =
    GtkThread2.sync begin fun () ->
      let row = model#append ?parent () in
      Gaux.may icon ~f:(model#set ~row ~column:col_icon);
      model#set ~row ~column:col_markup markup;
      row
    end ()

  method private append_struct ?parent item =
    match item.str_desc with
      | Tstr_eval expr -> ignore (self#append ?parent "Tstr_eval")
      | Tstr_value (_, pe) ->
        List.iter (fun (pat, expr) -> ignore (self#append_pattern ?parent pat.pat_desc expr.exp_desc)) pe
      | Tstr_class classes -> List.iter (self#append_class ?parent) classes
      | Tstr_type decls -> List.iter (self#append_type ?parent) decls
      | Tstr_exception (_, loc, decl) -> ignore (self#append ?parent ~icon:Icons.exc loc.txt)
      | Tstr_module (_, loc, module_expr) ->
        let icon =
          match module_expr.mod_desc with
            | Tmod_functor _ -> Icons.module_funct
            | _ -> Icons.module_impl
        in
        let parent = self#append ?parent ~icon (sprintf "<b>%s</b>" loc.txt) in
        self#append_module ~parent module_expr.mod_desc;
        view#expand_row (model#get_path parent);
      | _ -> ()

  method private append_module ?parent mod_desc =
    match mod_desc with
      | Tmod_functor (_, floc, mtype, mexpr) ->
        self#append_module ?parent mexpr.mod_desc
      | Tmod_structure str -> List.iter (self#append_struct ?parent) str.str_items
      | Tmod_ident _ -> ignore (self#append ?parent "Tmod_ident")
      | Tmod_apply _ -> ()
      | Tmod_constraint _ -> ignore (self#append ?parent "Tmod_constraint")
      | Tmod_unpack _ -> ignore (self#append ?parent "Tmod_unpack")

  method private append_type ?parent (_, loc, decl) =
    let icon =
      match decl.typ_kind with
        | Ttype_abstract -> Icons.type_abstract
        | Ttype_variant _ -> Icons.type_variant
        | Ttype_record _ -> Icons.type_record
    in
    ignore (self#append ?parent ~icon loc.txt)

  method private append_pattern ?parent pat_desc exp_desc =
    let is_function = match exp_desc with Texp_function _ -> true | _ -> false in
    match pat_desc with
      | Tpat_var (id, _) ->
        let icon = if is_function then Icons.func else Icons.simple in
        Some (self#append ?parent ~icon (Ident.name id))
      | _ -> None

  method private append_class_item ?parent = function
    | Tcl_structure str ->
      List.map begin fun fi ->
        match fi.cf_desc with
          | Tcf_inher (_, cl_expr, id, inherited_fields, inherited_methods) ->
            let parent = self#append_class_item ?parent cl_expr.cl_desc in
            List.iter (fun (x, _) -> ignore (self#append ?parent (sprintf "<i>%s</i>" x))) inherited_methods;
            parent
          | Tcf_init expr ->
            Some (self#append ~icon:Icons.init ?parent (sprintf "<i>initializer</i>:%s" (string_of_loc fi.cf_loc)));
          | Tcf_val (_, loc, mutable_flag, _, kind, _) ->
            let icon = match kind with
              | Tcfk_virtual _ when mutable_flag = Mutable -> Icons.attribute_mutable_virtual
              | Tcfk_virtual _  -> Icons.attribute_virtual
              | Typedtree.Tcfk_concrete _ when mutable_flag = Mutable -> Icons.attribute_mutable
              | Typedtree.Tcfk_concrete _ -> Icons.attribute
            in
            Some (self#append ?parent ~icon loc.txt (*(string_of_loc loc.loc)*));
          | Tcf_meth (_, loc, private_flag, kind, _) ->
            let icon = match kind with
              | Tcfk_virtual _ when private_flag = Private -> Icons.met_private_virtual
              | Tcfk_virtual _  -> Icons.met_virtual
              | Typedtree.Tcfk_concrete _ when private_flag = Private -> Icons.met_private
              | Typedtree.Tcfk_concrete _ -> Icons.met
            in
            Some (self#append ?parent ~icon loc.txt (*(string_of_loc loc.loc)*));
          | _ -> Some (self#append ?parent (sprintf "Tcf_..."));
      end str.cstr_fields;
      parent
    | Tcl_fun (_, _, _, cl_expr, _) ->
      self#append_class_item ?parent cl_expr.cl_desc;
    | Tcl_ident (_, lid, _) ->
      Some (self#append ?parent ~icon:Icons.class_inherit (String.concat "." (Longident.flatten lid.txt)));
    | Tcl_apply (cl_expr, _) ->
      self#append_class_item ?parent cl_expr.cl_desc;
    | Tcl_let (_, _, lets, desc) ->
      List.iter begin fun (_, loc, expr) ->
        let is_function = match expr.exp_desc with Texp_function _ -> true | _ -> false in
        let icon = if is_function then Icons.func else Icons.simple in
        ignore (self#append ~icon ?parent loc.txt)
      end lets;
      self#append_class_item ?parent desc.cl_desc;
    | Tcl_constraint (cl_expr, _, _, _, _) ->
      self#append_class_item ?parent cl_expr.cl_desc;

  method private append_class ?parent (infos, _, _) =
    let icon = if infos.ci_virt = Asttypes.Virtual then Icons.class_virtual else Icons.classe in
    let parent = self#append ~icon (sprintf "<b>%s</b>" infos.ci_id_name.txt) in
    ignore (self#append_class_item ~parent infos.ci_expr.cl_desc);
    view#expand_row (model#get_path parent);

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
