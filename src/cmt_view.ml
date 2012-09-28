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
open Types

type kind =
  | Function
  | Simple
  | Class
  | Class_virtual
  | Class_type
  | Class_inherit
  | Attribute
  | Attribute_mutable
  | Attribute_mutable_virtual
  | Attribute_virtual
  | Initializer
  | Method
  | Method_private
  | Method_virtual
  | Method_private_virtual
  | Method_inherited
  | Type
  | Type_abstract
  | Type_variant
  | Type_record
  | Module
  | Module_functor
  | Module_type
  | Exception
  | Error
  | Warning
  | Folder_warnings
  | Folder_errors
  | Dependencies
  | Bookmark of GdkPixbuf.pixbuf
  | Class_let_bindings
  | Unknown

let pixbuf_of_kind = function
  | Function -> Some Icons.func
  | Simple -> Some Icons.simple
  | Method -> Some Icons.met
  | Method_private -> Some Icons.met_private
  | Method_virtual -> Some Icons.met_virtual
  | Method_private_virtual -> Some Icons.met_private_virtual
  | Method_inherited -> Some Icons.met
  | Initializer -> Some Icons.init
  | Attribute -> Some Icons.attribute
  | Attribute_mutable -> Some Icons.attribute_mutable
  | Attribute_mutable_virtual -> Some Icons.attribute_mutable_virtual
  | Attribute_virtual -> Some Icons.attribute_virtual
  | Type -> Some Icons.typ
  | Type_abstract -> Some Icons.type_abstract
  | Type_variant -> Some Icons.type_variant
  | Type_record -> Some Icons.type_record
  | Class -> Some Icons.classe
  | Class_virtual -> Some Icons.class_virtual
  | Class_type -> Some Icons.class_type
  | Class_inherit -> Some Icons.class_inherit
  | Module -> Some Icons.module_impl
  | Module_functor -> Some Icons.module_funct
  | Module_type -> Some Icons.module_type
  | Exception -> Some Icons.exc
  | Error -> Some Icons.error_14
  | Warning -> Some Icons.warning_14
  | Folder_warnings -> Some Icons.folder_warning
  | Folder_errors -> Some Icons.folder_error
  | Dependencies -> None
  | Bookmark pixbuf -> Some pixbuf
  | Class_let_bindings -> None
  | Unknown -> None;;


let cols              = new GTree.column_list
let col_icon          = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf")
let col_markup        = cols#add Gobject.Data.string
let col_loc           = cols#add Gobject.Data.caml
let col_loc_body      = cols#add Gobject.Data.caml
let col_lazy : (unit -> unit) list GTree.column         = cols#add Gobject.Data.caml
(*let col_name          = cols#add Gobject.Data.string
let col_name_sort     = cols#add Gobject.Data.string
let col_type          = cols#add Gobject.Data.string
let col_id            = cols#add Gobject.Data.int*)

let string_of_loc loc =
  let filename, a, b = Location.get_pos_info loc.loc_start in
  let _, c, d = Location.get_pos_info loc.loc_end in
  sprintf "%s, %d:%d -- %d:%d" filename a b c d;;

let linechar_of_loc loc =
  let _, a, b = Location.get_pos_info loc.loc_start in
  let _, c, d = Location.get_pos_info loc.loc_end in
  ((a - 1), b), ((c - 1), d)

let is_function type_expr =
  let rec f t =
    match t.Types.desc with
      | Types.Tarrow _ -> true
      | Types.Tlink t -> f t
      | _ -> false
  in
  f type_expr;;

let string_of_type_expr te =
  match te.desc with
    | Tarrow (_, _, t2, _) -> Odoc_info.string_of_type_expr t2
    | _ -> Odoc_info.string_of_type_expr te

class widget ~editor ~page ?packing () =
  let sw                = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ?packing () in
  let model             = GTree.tree_store cols in
  let view              = GTree.view ~model ~headers_visible:false ~packing:sw#add ~width:350 ~height:500 () in
  let renderer_pixbuf   = GTree.cell_renderer_pixbuf [`YPAD 0; `XPAD 0] in
  let renderer_markup   = GTree.cell_renderer_text [`YPAD 0] in
  let vc                = GTree.view_column () in
  let _                 = vc#pack ~expand:false renderer_pixbuf in
  let _                 = vc#pack ~expand:false renderer_markup in
  let _                 = vc#add_attribute renderer_pixbuf "pixbuf" col_icon in
  let _                 = vc#add_attribute renderer_markup "markup" col_markup in
  let _                 = view#selection#set_mode `SINGLE in
  let _                 = view#append_column vc in
  let _                 = view#misc#set_property "enable-tree-lines" (`BOOL true) in
  let _                 = view#misc#modify_font_by_name Preferences.preferences#get.Preferences.pref_compl_font in
  let _                 = view#misc#modify_base [`SELECTED, `NAME Oe_config.outline_selection_bg_color; `ACTIVE, `NAME Oe_config.outline_active_bg_color] in
  let _                 = view#misc#modify_text [`SELECTED, `NAME Oe_config.outline_selection_fg_color; `ACTIVE, `NAME Oe_config.outline_active_fg_color] in
  let type_color        = Oe_config.outline_type_color in
  let type_color_re     = Str.regexp_string type_color in
  let type_color_sel    = Color.name_of_gdk (view#misc#style#fg `SELECTED) in
  let type_color_sel_re = Str.regexp_string type_color_sel in
  let buffer : Ocaml_text.buffer = page#buffer in
object (self)
  inherit GObj.widget sw#as_widget
  val changed = new changed()
  val mutable last_selected_path = None
  val mutable signal_selection_changed = None
  val mutable timestamp = "", 0.0
  val mutable filename = ""
  val mutable marks : (string * Gtk.text_mark) list = []

  initializer
    (** Replace foreground color whene row is selected *)
    let replace_color_in_markup (model : GTree.tree_store) invert path =
      let row = model#get_iter path in
      let markup = model#get ~row ~column:col_markup in
      let new_markup = if invert then begin
        Str.replace_first type_color_sel_re type_color markup
      end else begin
        Str.replace_first type_color_re type_color_sel markup
      end in
      model#set ~row ~column:col_markup new_markup;
    in
    ignore (view#selection#connect#changed ~callback:begin fun () ->
      match view#selection#get_selected_rows with
        | path :: _ ->
          Gaux.may last_selected_path ~f:(replace_color_in_markup model true);
          replace_color_in_markup model false path;
          last_selected_path <- Some path;
        | _ -> ()
    end);
    (** Tooltips *)
    view#misc#set_has_tooltip true;
    ignore (view#misc#connect#query_tooltip ~callback: begin fun ~x ~y ~kbd tooltip ->
      try
        begin
          match GtkTree.TreeView.Tooltip.get_context view#as_tree_view ~x ~y ~kbd with
            | (x, y, Some (_, _, row)) ->
              begin
                match view#get_path_at_pos ~x ~y with
                  | Some (tpath, _, _, _) ->
                    let markup = model#get ~row ~column:col_markup in
                    let pos = String.index markup ':' + 1 in
                    let markup = (Str.string_before markup pos) ^ "\n" ^ (Str.string_after markup pos) in
                    let markup = sprintf "<big>%s</big>" markup in
                    GtkBase.Tooltip.set_markup tooltip markup;
                    (*let label = GMisc.label ~markup () in
                    GtkBase.Tooltip.set_custom tooltip label#as_widget;*) (* This flickers on some platforms *)
                    GtkTree.TreeView.Tooltip.set_row view#as_tree_view tooltip tpath;
                    true
                  | _ -> false
              end
            | _ -> false
        end
      with Not_found | Gpointer.Null -> false
    end);
    (** Events *)
    signal_selection_changed <- Some (view#selection#connect#after#changed ~callback:self#select_element);
    ignore (view#connect#after#row_activated ~callback:begin fun _ _ ->
      self#select_element();
      page#view#misc#grab_focus();
    end);
    ignore (view#selection#connect#changed ~callback:begin fun () ->
      match view#selection#get_selected_rows with
        | path :: _ ->
          let row = model#get_iter path in
          begin
            try List.iter (fun f -> f()) (List.rev (model#get ~row ~column:col_lazy));
            with Failure _ -> ()
          end;
          model#set ~row ~column:col_lazy [];
        | _ -> ()
    end);
    (*ignore (view#misc#connect#realize ~callback:begin fun () ->
      let show = Preferences.preferences#get.Preferences.pref_outline_show_types in
      if show <> button_show_types#active then button_show_types#clicked()
    end);*)
    (**  *)
    self#load()

  method view = view

  method select ?(align : float option) (mark : Gtk.text_mark) = ()

  method select_element () =
    match view#selection#get_selected_rows with
      | [] -> ()
      | path :: _ ->
        let row = model#get_iter path in
        Gaux.may (model#get ~row ~column:col_loc) ~f:begin fun loc ->
          let ts_filename, ts_time = timestamp in
          if ts_filename = filename && ts_time = (Unix.stat filename).Unix.st_mtime then begin
            let start, stop = linechar_of_loc loc in
            let where = buffer#get_iter (`LINECHAR start) in
            let stop = buffer#get_iter (`LINECHAR stop) in
            buffer#select_range where stop;
            page#view#scroll_lazy where;
          end else begin
            match List_opt.assoc (GTree.Path.to_string path) marks with
              | Some mark when not (GtkText.Mark.get_deleted mark) ->
                let where = buffer#get_iter_at_mark (`MARK mark) in
                buffer#place_cursor ~where;
                page#view#scroll_lazy where;
              | _ -> ()
          end
        end

  method load () =
    let source = page#get_filename in
    match Project.tmp_of_abs editor#project source with
      | Some (tmp, relname) ->
        Miscellanea.crono  self#load' (tmp // relname);
      | _ -> ()

  method private load' file =
    filename <- file;
    let filename_cmt = (Filename.chop_extension filename) ^ ".cmt" in
    timestamp <- file, (Unix.stat filename).Unix.st_mtime;
    let cmt = Cmt_format.read_cmt filename_cmt in
    GtkThread2.sync begin fun () ->
      buffer#block_signal_handlers ();
      List.iter (fun (_, mark) -> (*GtkThread2.sync*) buffer#delete_mark (`MARK mark)) marks;
      buffer#unblock_signal_handlers ();
      marks <- [];
    end ();
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

  method private append ?parent ?kind ?loc ?loc_body name typ =
    let markup_name =
      match kind with
        | Some Class | Some Class_virtual | Some Class_type | Some Module | Some Module_functor ->
          "<b>" ^ (Glib.Markup.escape_text name) ^ "</b>"
        | Some Initializer | Some Class_let_bindings | Some Method_inherited -> "<i>" ^ (Glib.Markup.escape_text name) ^ "</i>"
        | _ -> Glib.Markup.escape_text name
    in
    let typ_utf8 = Glib.Convert.convert_with_fallback ~fallback:"" ~from_codeset:Oe_config.ocaml_codeset ~to_codeset:"UTF-8" typ in
    let markup = if typ = "" then markup_name else String.concat "" [
      markup_name; " <span color='"; type_color; "'> : ";
      (Print_type.markup2 (Miscellanea.replace_all ~regexp:true ["\n", ""; " +", " "] typ_utf8));
      "</span>"
    ] in
    GtkThread2.sync begin fun () ->
      let row = model#append ?parent () in
      model#set ~row ~column:col_markup markup;
      model#set ~row ~column:col_loc loc;
      model#set ~row ~column:col_loc_body loc_body;
      Gaux.may kind ~f:(fun k -> Gaux.may (pixbuf_of_kind k) ~f:(model#set ~row ~column:col_icon));

      begin
        match loc with
          | Some loc ->
              let (line, chars), _ = linechar_of_loc loc in
              if line < buffer#end_iter#line + 1 then begin
                let iter : GText.iter = buffer#get_iter (`LINE (line - 1)) in
                let iter = iter#forward_chars chars in
                if iter#line + 1 = line then begin
                  buffer#block_signal_handlers ();
                  let mark = buffer#create_mark(* ~name:(Gtk_util.create_mark_name "Outline.set_location")*) ?left_gravity:None iter in
                  marks <- (GTree.Path.to_string (model#get_path row), mark) :: marks;
                  buffer#unblock_signal_handlers ()
                end
              end
          | _ -> ()
      end;
      row
    end ()

  method private append_struct ?parent item =
    match item.str_desc with
      | Tstr_eval expr ->
        let loc = {item.str_loc with loc_end = item.str_loc.loc_start} in
        ignore (self#append ?parent ~loc ~loc_body:expr.exp_loc "_" "")
      | Tstr_value (_, pe) ->
        List.iter (fun (pat, _) -> ignore (self#append_pattern ?parent pat)) pe
      | Tstr_class classes -> List.iter (self#append_class ?parent) classes
      | Tstr_class_type classes -> List.iter (fun (_, loc, decl) -> self#append_class_type ?parent ~loc decl) classes
      | Tstr_type decls -> List.iter (self#append_type ?parent) decls
      | Tstr_exception (_, loc, decl) ->
        let exn_params = self#string_of_core_types decl.exn_params in
        ignore (self#append ?parent ~kind:Exception ~loc:loc.loc loc.txt exn_params)
      | Tstr_module (_, loc, module_expr) ->
        let kind =
          match module_expr.mod_desc with
            | Tmod_functor _ -> Module_functor
            | _ -> Module
        in
        let parent_mod = self#append ?parent ~kind ~loc:loc.loc loc.txt "" in
        let f () = self#append_module ~parent:parent_mod module_expr.mod_desc in
        begin
          match parent with
            | Some _ -> model#set ~row:parent_mod ~column:col_lazy [f];
            | _ -> f()
        end;
        view#expand_row (model#get_path parent_mod);
      | Tstr_modtype (_, loc, mt) ->
        let parent = self#append ?parent ~kind:Module_type ~loc:loc.loc loc.txt "" in
        model#set ~row:parent ~column:col_lazy [fun () -> self#append_module_type ~parent mt.mty_desc];
      | Tstr_recmodule _ -> ()
      | Tstr_include _ | Tstr_open _ | Tstr_exn_rebind _ | Tstr_primitive _ -> ()

  method private append_sig ?parent item =
    match item.sig_desc with
      | Tsig_value _ -> ignore (self#append ?parent "Tsig_value" "")
      | Tsig_type _ -> ignore (self#append ?parent "Tsig_type" "")
      | Tsig_exception _ -> ignore (self#append ?parent "Tsig_exception" "")
      | Tsig_module _ -> ignore (self#append ?parent "Tsig_module" "")
      | Tsig_recmodule _ -> ignore (self#append ?parent "Tsig_recmodule" "")
      | Tsig_modtype _ -> ignore (self#append ?parent "Tsig_modtype" "")
      | Tsig_open _ -> ignore (self#append ?parent "Tsig_open" "")
      | Tsig_include _ -> ignore (self#append ?parent "Tsig_include" "")
      | Tsig_class _ -> ignore (self#append ?parent "Tsig_class" "")
      | Tsig_class_type decls ->
        List.iter (fun info -> self#append_class_type ?parent ~loc:info.ci_id_name info) decls;

  method private append_module ?parent mod_desc =
    match mod_desc with
      | Tmod_functor (_, floc, mtype, mexpr) ->
        self#append_module ?parent mexpr.mod_desc
      | Tmod_structure str ->
        List.iter (self#append_struct ?parent) str.str_items
      | Tmod_ident _ -> ignore (self#append ?parent "Tmod_ident" "")
      | Tmod_apply _ -> ()
      | Tmod_constraint _ -> ignore (self#append ?parent "Tmod_constraint" "")
      | Tmod_unpack _ -> ignore (self#append ?parent "Tmod_unpack" "")

  method private append_module_type ?parent mod_desc =
    match mod_desc with
      | Tmty_functor (_, floc, mtype, mexpr) ->
        self#append_module_type ?parent mexpr.mty_desc
      | Tmty_ident (path, loc) -> ignore (self#append ?parent ~loc:loc.loc "Tmty_ident" "")
      | Tmty_signature sign ->
        List.iter (self#append_sig ?parent) sign.sig_items
      | Tmty_with _ -> ignore (self#append ?parent "Tmty_with" "")
      | Tmty_typeof _ -> ignore (self#append ?parent "Tmty_typeof" "")

  method private append_type ?parent (_, loc, decl) =
    let kind =
      match decl.typ_kind with
        | Ttype_abstract -> Type_abstract
        | Ttype_variant _ -> Type_variant
        | Ttype_record _ -> Type_record
    in
    let typ =
      match decl.typ_kind with
        | Ttype_abstract -> self#string_of_type_abstract decl
        | Ttype_variant decls -> self#string_of_type_variant decls
        | Ttype_record decls -> self#string_of_type_record decls
    in
    ignore (self#append ?parent ~kind ~loc:loc.loc loc.txt typ)

  method private append_pattern ?parent pat =
    match pat.pat_desc with
      | Tpat_var (_, loc) ->
        let kind = if is_function pat.pat_type then Function else Simple in
        Odoc_info.reset_type_names();
        Some (self#append ?parent ~kind ~loc:loc.loc loc.txt (string_of_type_expr pat.pat_type))
      | Tpat_tuple pats ->
        List.iter (fun pat -> ignore (self#append_pattern ?parent pat)) pats;
        None
      | (*Tpat_construct _ | Tpat_any | Tpat_alias _ | Tpat_constant _
      | Tpat_variant _ | Tpat_record _ | Tpat_array _ | Tpat_or _
      | Tpat_lazy*) _ -> None

  method private append_class ?parent (infos, _, _) =
    let kind = if infos.ci_virt = Asttypes.Virtual then Class_virtual else Class in
    let parent = self#append ~kind ?parent ~loc:infos.ci_id_name.loc infos.ci_id_name.txt "" in
    let let_bindings_parent = self#append ~kind:Class_let_bindings ~loc:infos.ci_id_name.loc ~parent "let-bindings" "" in
    let count_meth = ref 0 in
    ignore (self#append_class_item ~let_bindings_parent ~count_meth ~parent infos.ci_expr.cl_desc);
    let has_childs =
      try (model#get ~row:let_bindings_parent ~column:col_lazy) <> [] with Failure _ -> false
    in
    if not has_childs then (ignore (model#remove let_bindings_parent));
    if !count_meth > 0 then begin
      Gmisclib.Idle.add ~prio:100 begin fun () ->
        try view#expand_row (model#get_path parent) with Gpointer.Null -> ()
      end;
    end

  method private append_class_item ?let_bindings_parent ?count_meth ?parent = function
    | Tcl_structure str ->
      List.map begin fun fi ->
        match fi.cf_desc with
          | Tcf_inher (_, cl_expr, id, inherited_fields, inherited_methods) ->
            let parent = self#append_class_item ?let_bindings_parent ?count_meth ?parent cl_expr.cl_desc in
            Gaux.may parent ~f:begin fun parent ->
              model#set ~row:parent ~column:col_lazy
                [fun () -> List.iter (fun (x, _) -> ignore (self#append ~kind:Method_inherited ~parent x "")) inherited_methods];
            end;
            parent
          | Tcf_init _ ->
            let loc = {fi.cf_loc with loc_end = fi.cf_loc.loc_start} in
            Some (self#append ~kind:Initializer ~loc ?parent "initializer" "");
          | Tcf_val (_, loc, mutable_flag, _, kind, _) ->
            let typ, kind = match kind with
              | Tcfk_virtual ct when mutable_flag = Mutable ->
                string_of_type_expr ct.ctyp_type, Attribute_mutable_virtual
              | Tcfk_virtual ct  ->
                string_of_type_expr ct.ctyp_type, Attribute_virtual
              | Tcfk_concrete te when mutable_flag = Mutable ->
                string_of_type_expr te.exp_type, Attribute_mutable
              | Tcfk_concrete te ->
                string_of_type_expr te.exp_type, Attribute
            in
            Some (self#append ?parent ~kind ~loc:loc.loc loc.txt typ);
          | Tcf_meth (_, loc, private_flag, kind, _) ->
            let kind, typ, loc_body = match kind with
              | Tcfk_virtual ct when private_flag = Private ->
                Method_private_virtual, string_of_type_expr ct.ctyp_type, ct.ctyp_loc
              | Tcfk_virtual ct ->
                Method_virtual, string_of_type_expr ct.ctyp_type, ct.ctyp_loc
              | Tcfk_concrete te when private_flag = Private ->
                Method_private, string_of_type_expr te.exp_type, te.exp_loc
              | Tcfk_concrete te ->
                Method, string_of_type_expr te.exp_type, te.exp_loc
            in
            Gaux.may count_meth ~f:(fun x -> incr x);
            Some (self#append ?parent ~kind ~loc:loc.loc ~loc_body:loc_body loc.txt typ);
          | Tcf_constr (ct1, _) -> Some (self#append ?parent ~loc:ct1.ctyp_loc (sprintf "Tcf_constr") "");
      end str.cstr_fields;
      parent
    | Tcl_fun (_, _, _, cl_expr, _) ->
      self#append_class_item ?let_bindings_parent ?count_meth ?parent cl_expr.cl_desc;
    | Tcl_ident (_, lid, _) ->
      Some (self#append ?parent ~kind:Class_inherit ~loc:lid.loc (String.concat "." (Longident.flatten lid.txt)) "");
    | Tcl_apply (cl_expr, _) ->
      self#append_class_item ?let_bindings_parent ?count_meth ?parent cl_expr.cl_desc;
    | Tcl_let (_, lets, _, desc) ->
      Gaux.may let_bindings_parent ~f:begin fun row ->
        let prev = try model#get ~row ~column:col_lazy with Failure _ -> [] in
        model#set ~row ~column:col_lazy ((fun () ->
          List.iter begin fun (pat, expr) ->
            ignore (self#append_pattern ~parent:row pat);
          end lets;) :: prev)
      end;
      self#append_class_item ?let_bindings_parent ?count_meth ?parent desc.cl_desc;
    | Tcl_constraint (cl_expr, _, _, _, _) ->
      self#append_class_item ?let_bindings_parent ?count_meth ?parent cl_expr.cl_desc;

  method private append_class_type ?parent ~loc infos =
    let parent = self#append ~kind:Class_type ?parent ~loc:loc.loc loc.txt "" in
    ignore (self#append_class_type_item ~parent infos.ci_expr.cltyp_desc);

  method private append_class_type_item ?parent = function
    | Tcty_constr _ -> ignore (self#append ?parent (sprintf "Tcty_constr") "")
    | Tcty_signature sign ->
      List.iter begin fun field ->
        match field.ctf_desc with
          | Tctf_inher _ -> ignore (self#append ?parent (sprintf "Tctf_inher") "")
          | Tctf_val _ -> ignore (self#append ?parent (sprintf "Tctf_val") "")
          | Tctf_virt _ -> ignore (self#append ?parent (sprintf "Tctf_virt") "")
          | Tctf_meth (id, private_flag, ct) ->
            let kind = match private_flag with Private -> Method_private | _ -> Method in
            ignore (self#append ?parent ~kind id (string_of_type_expr ct.ctyp_type))
          | Tctf_cstr _ -> ignore (self#append ?parent (sprintf "Tctf_cstr") "")
      end sign.csig_fields
    | Tcty_fun (_, _, class_type) -> ignore (self#append_class_type_item ?parent class_type.cltyp_desc)

  method private string_of_type_abstract decl =
    match decl.typ_manifest with
      | Some ct -> Odoc_info.string_of_type_expr ct.ctyp_type
      | _ -> ""

  method private string_of_type_record decls =
    Odoc_info.reset_type_names();
    String.concat "" ["{ \n";
      String.concat "; \n" (List.map begin fun (_, loc, _is_mutable, ct, _) ->
        loc.txt ^ ": " ^ string_of_type_expr ct.ctyp_type
      end decls);
      " \n}"]

  method private string_of_type_variant decls =
    Odoc_info.reset_type_names();
    String.concat "\n | " (List.map begin fun (_, loc, types, _) ->
      loc.txt ^
        let ts = self#string_of_core_types types in
        if ts = "" then "" else (" of " ^ ts)
    end decls)

  method private string_of_core_types ctl =
    String.concat " * " (List.map (fun ct -> string_of_type_expr ct.ctyp_type) ctl)

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
