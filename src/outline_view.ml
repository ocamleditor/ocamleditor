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

module ColorOps = Color
open Preferences
open Cmt_format
open Typedtree

module TI = Tast_iterator

module Log = Common.Log.Make(struct let prefix = "Outline_view" end)
let _ = Log.set_verbosity `DEBUG


(** [Lexing.postion] copy *)
type position = Lexing.position = {
  pos_fname : string;
  pos_lnum  : int;
  pos_bol   : int;
  pos_cnum  : int;
}

(** [Location.t] copy *)
type location = Location.t = {
  loc_start : position;
  loc_end   : position;
  loc_ghost : bool;
}

(** ['a Location.loc] & ['a Asttypes.loc] copy *)
type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : location;
}

let cols               = new GTree.column_list
let col_icon           = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf")
let col_name           = cols#add Gobject.Data.string
let col_markup         = cols#add Gobject.Data.string
let col_loc            = cols#add Gobject.Data.int
let col_tooltip        = cols#add Gobject.Data.string
let col_default_sort   = cols#add Gobject.Data.int

let is_function type_expr =
  let rec f t =
    match [@warning "-4"] Types.get_desc t with
    | Types.Tarrow _ -> true
    | Types.Tlink t -> f t
    | _ -> false
  in f type_expr;;

let string_of_longident t
  = String.concat "." @@ Longident.flatten t

let flatten_formatted te =
  let buf = Buffer.create 64 in
  let is_space = ref false in
  String.iter (fun ch ->
      ( match !is_space, ch with
        | false, ch when ch = '\n' || ch = ' ' -> is_space := true; Buffer.add_char buf ' ';
        | false, ch -> Buffer.add_char buf ch

        | true, ch when ch = '\n' || ch = ' ' -> ()
        | true, ch -> is_space := false; Buffer.add_char buf ch
      )) te;
  Buffer.contents buf

(** empty *)
let empty () =
  let open! Settings_j in
  let pref = Preferences.preferences#get in
  let vp = GBin.viewport () in
  let label = GMisc.label ~xalign:0.5 ~yalign:0. ~xpad:3 ~ypad:3
      ~text:"Structure is not available" ~packing:vp#add () in
  vp#misc#modify_bg [`NORMAL, `NAME ?? (pref.outline_color_nor_bg)];
  label#misc#modify_fg [
    `NORMAL, `NAME ?? (pref.outline_color_nor_fg)
  ];
  vp#coerce;;

let count = ref 0
let parent_row = ref None

let model_clear (model : GTree.tree_store) =
  count := 0;
  parent_row := None;
  model#clear ()

let model_append
    ( model : GTree.tree_store )
    ?icon
    ?(loc = 0)
    ?(tooltip = "TODO")
    ( text : string )
  =
  let parent = !parent_row in
  let row = model#append ?parent () in
  let () = Option.iter (fun icon -> model#set ~row ~column:col_icon (??? icon)) icon in
  let () = model#set ~row ~column:col_markup text in
  let () = model#set ~row ~column:col_loc loc in
  let () = model#set ~row ~column:col_tooltip tooltip in
  let () = model#set ~row ~column:col_default_sort !count in
  incr count;
  row

let update_parent_markup
    ( model : GTree.tree_store )
    ( update_fn : string -> string )
  =
  match !parent_row with
  | Some row ->
      let markup = update_fn (model#get ~row ~column:col_markup) in
      model#set ~row ~column:col_markup markup
  | None -> ()

let b text = "<b>" ^ text ^ "</b>"
let i text = "<i>" ^ text ^ "</i>"

let string_of_id ?(default="") id =
  id |> Option.map Ident.name |> Option.value ~default

let string_of_type_expr ?(is_method=false) te =
  let te = match Types.get_desc te with
    | Types.Tarrow (_, _self, te2, _) when is_method -> te2
    | _ -> te
  in
  Odoc_info.reset_type_names ();
  Odoc_info.string_of_type_expr te
  |> Print_type.markup2

let string_of_functor_parameter = function
  | Typedtree.Unit -> "()"
  | Typedtree.Named (id, _, md_type) ->
      let md_name = string_of_id id in
      let { mty_type; _ } = md_type in
      let md_type = Odoc_info.string_of_module_type mty_type in
      "(" ^ md_name ^ " : TODO " ^ md_type ^ ")"

let outline_iterator (model : GTree.tree_store) =
  let super = TI.default_iterator in

  let expr : expression option ref = ref None in
  let parameter : bool ref = ref false in

  let id_name id =
    Ident.name id |> (if !parameter then i else b),
    if !parameter then None else Some Icons.simple
  in
  let lid_name lid =
    string_of_longident lid |> (if !parameter then i else b),
    if !parameter then None else Some Icons.simple
  in

  let append ?loc text = model_append model ?loc text |> ignore in
  let structure_item iterator (item : structure_item) =
    let { str_desc = desc; str_loc; _ } = item in
    let loc = str_loc.loc_start.pos_cnum in
    ( match desc with
      | Tstr_eval (_, _) -> append ~loc "_ (eval)"
      | Tstr_value (is_rec, vb) ->
          if is_rec = Asttypes.Recursive then (
            let parent = !parent_row in
            parent_row :=  Some (model_append model (b "rec"));
            List.iter (iterator.TI.value_binding iterator) vb;
            parent_row := parent
          )
          else
            List.iter (iterator.TI.value_binding iterator) vb

      | Tstr_primitive _ -> append ~loc "_ (primitive)"
      | Tstr_type (_, _) -> append ~loc "_ (type)"
      | Tstr_typext _ -> append ~loc "_ (typext)"
      | Tstr_exception _ -> append ~loc "_ (exception)"
      | Tstr_module mb -> iterator.TI.module_binding iterator mb
      | Tstr_recmodule _ -> append ~loc "_ (rec module)"
      | Tstr_modtype _ -> append ~loc "_ (modtype)"
      | Tstr_open _ -> append ~loc "_ (open)"
      | Tstr_class classes ->
          List.iter
            ( fun (cls, _) ->
                let { ci_id_name; _ } = cls in
                let { txt; loc } = ci_id_name in
                let loc = loc.loc_start.pos_cnum in

                let parent = !parent_row in
                parent_row := Some (model_append model ~loc ~icon:Icons.classe (b txt));
                iterator.TI.class_declaration iterator cls;

                parent_row := parent;
            )
            classes
      | Tstr_class_type decls ->
          List.iter
            ( fun (cls_id, _, item) ->
                let parent = !parent_row in
                parent_row := Some (model_append model ~loc ~icon:Icons.class_type (b @@ Ident.name cls_id));
                iterator.TI.class_type_declaration iterator item;
                parent_row := parent
            )
            decls
      | Tstr_include _ -> append ~loc "_ (include)"
      | Tstr_attribute _ -> append ~loc "_ (attribute)"
    )
  in

  let rec class_expr iterator ( item : class_expr ) let_bindings =
    let { cl_desc; cl_loc; _ } = item in
    let loc  = cl_loc.loc_start.pos_cnum in
    ( match cl_desc with
      | Tcl_ident _ -> append ~loc "Tcl_ident - TODO";
      | Tcl_structure item ->
          iterator.TI.class_structure iterator item
      | Tcl_fun (_, pat, _, cl_expr, _ ) ->
          (* TODO: Do somethign about the label *)
          parameter := true;
          iterator.TI.pat iterator pat;
          parameter := false;

          iterator.TI.class_expr iterator cl_expr

      | Tcl_apply _ -> append ~loc "Tcl_apply - TODO"

      | Tcl_let (_rec_flag, bindings, _, cl_expr) ->
          let { cl_desc; _ } = cl_expr in
          ( match cl_desc with
            | Tcl_let (_, inner_bindings, _, cl_expr) ->
                class_expr iterator cl_expr (let_bindings @ bindings @ inner_bindings)
            | _ ->
                let parent = !parent_row in
                parent_row := Some (model_append model ~loc ~tooltip:"" (Glib.Markup.escape_text "<let bindings>"));
                List.iter (iterator.TI.value_binding iterator) (let_bindings @ bindings);
                parent_row := parent;

                iterator.TI.class_expr iterator cl_expr
          )

      | Tcl_constraint _ -> append ~loc "Tcl_constraint - TODO"
      | Tcl_open _ -> append ~loc "Tcl_open - TODO"
    )
  in
  let class_expr iterator item = class_expr iterator item [] in

  let class_structure iterator ( item : class_structure ) =
    let { cstr_self; cstr_fields; _ } = item in
    parameter := true;
    iterator.TI.pat iterator cstr_self;
    parameter := false;

    List.iter
      (iterator.TI.class_field iterator)
      cstr_fields
  in

  let class_field iterator ( item : class_field ) =
    let { cf_desc; cf_loc; _ } = item in
    let loc = cf_loc.loc_start.pos_cnum in

    let field_type ?is_method kind =
      ( match kind with
        | Tcfk_virtual _core_type -> "virtual -- TODO"
        | Tcfk_concrete (_, { exp_type; _ }) -> string_of_type_expr ?is_method exp_type
      )
    in
    let field_icon kind flag =
      ( match kind, flag with
        | Tcfk_virtual _, Asttypes.Mutable -> Icons.attribute_mutable_virtual
        | Tcfk_virtual _, Asttypes.Immutable -> Icons.attribute_virtual
        | Tcfk_concrete _, Asttypes.Immutable -> Icons.attribute
        | Tcfk_concrete _, Asttypes.Mutable -> Icons.attribute_mutable
      )
    in
    let method_icon kind flag =
      ( match kind, flag with
        | Tcfk_virtual _, Asttypes.Public -> Icons.met_virtual
        | Tcfk_virtual _, Asttypes.Private -> Icons.met_private_virtual
        | Tcfk_concrete _, Asttypes.Public  -> Icons.met
        | Tcfk_concrete _, Asttypes.Private -> Icons.met_private
      )
    in

    ( match cf_desc with
      | Tcf_inherit _ -> append ~loc "Tcf_inherit - TODO"
      | Tcf_val ( { txt; _ }, flag, _, kind, _ ) ->
          let field_type = field_type kind in
          let icon = field_icon kind flag in
          model_append model ~loc ~icon ~tooltip:field_type  (i txt ^ " : " ^ field_type) |> ignore
      | Tcf_method ( { txt; _ }, priv, kind ) ->
          let field_type = field_type ~is_method:true kind in
          let icon = method_icon kind priv in
          model_append model ~loc ~icon ~tooltip:field_type (b txt ^ " : " ^ field_type) |> ignore
      | Tcf_constraint _ -> append ~loc "Tcf_constraint - TODO"
      | Tcf_initializer _ ->
          model_append model ~loc ~icon:Icons.grip ~tooltip:"" (b "initializer") |> ignore
      | Tcf_attribute _ -> append ~loc "Tcf_attribute - TODO"
    )
  in

  let module_binding iterator (mb : module_binding) =
    let { mb_id; mb_expr; mb_loc; _ } = mb in
    let text = string_of_id mb_id in
    let loc = mb_loc.loc_start.pos_cnum in

    let parent = !parent_row in
    let row =  model_append model ~icon:Icons.module_impl ~loc (b text) in
    parent_row := Some row;
    iterator.TI.module_expr iterator mb_expr;
    parent_row := parent
  in

  let module_expr
      ( iterator : TI.iterator )
      { mod_desc; _ }
    =
    match mod_desc with
    | Tmod_ident (_, { txt; _ }) ->
        let alias = String.concat "." @@ Longident.flatten txt in
        model_append model ~icon:Icons.simple (i alias) |> ignore
    | Tmod_structure structure ->
        iterator.TI.structure iterator structure;
    | Tmod_functor (p, expr) ->
        let param = string_of_functor_parameter p in
        update_parent_markup model (fun name -> name ^ " " ^ param) ;
        iterator.TI.module_expr iterator expr;
    | Tmod_apply (m1, m2, _) ->
        iterator.TI.module_expr iterator m1;
        let row = model_append model ~icon:Icons.module_impl ~tooltip:"" "" in
        let parent = !parent_row in
        parent_row := Some row;
        iterator.TI.module_expr iterator m2;
        parent_row := parent
    | Tmod_constraint (_, _, _, _) ->
        model_append model ~icon:Icons.simple "Constraint" |> ignore;
    | Tmod_unpack (_, _) ->
        model_append model ~icon:Icons.simple "Unpack What" |> ignore
  in

  let value_binding
      ( iterator : Tast_iterator.iterator )
      { vb_pat; vb_expr; _ }
    =
    expr := Some vb_expr;
    iterator.TI.pat iterator vb_pat;
    expr := None
  in

  let pat iterator (type k) (pattern : k Typedtree.general_pattern) =
    let { pat_desc = desc; pat_loc; pat_type; _} = pattern in
    let loc = pat_loc.loc_start.pos_cnum in
    ( match desc with
      (* value patterns *)
      | Tpat_any -> ()  (* let _ = .. *)
      | Tpat_constant _ -> () (* let () = .. *)
      | Tpat_var (id, _) ->
          let tooltip = string_of_type_expr pat_type in
          let name, icon = id_name id in
          let flat_type = flatten_formatted tooltip in
          let markup = name ^ " : " ^ flat_type in
          if is_function pat_type && Option.is_some !expr then (
            let parent = !parent_row in
            parent_row := Some (model_append model ?icon ~loc ~tooltip markup);
            iterator.TI.expr iterator (Option.get !expr);
            parent_row := parent
          )
          else
            model_append model ~loc ?icon ~tooltip markup |> ignore;

      | Tpat_alias (al_pat, id, _) ->
          iterator.TI.pat iterator al_pat ;
          let tooltip = string_of_type_expr pat_type in
          let name, icon = id_name id in
          if String.contains name '-' then
            ignore name (* object utility aliases *)
          else (
            let flat_type = flatten_formatted tooltip in
            let markup = name ^ " : " ^ flat_type in
            model_append model ~loc ?icon ~tooltip markup |> ignore
          )

      | Tpat_construct (lid, cd, pats, params_opt) ->
          let { txt; loc } = lid in
          let loc = loc.loc_start.pos_cnum in
          let name, icon = lid_name txt in
          model_append model ~loc ?icon (name ^ " ??") |> ignore

      | Tpat_tuple pats ->
          (* TODO: maybe insert parent row for tuple, with just an icon *)
          List.iter (iterator.TI.pat iterator) pats
      | Tpat_variant _ -> model_append model ~loc "_ (pat variant)" |> ignore
      | Tpat_record (fields, _) ->
          (* TODO: maybe insert parent row for record, with just an icon *)
          List.iter (
            fun (_, _, lb_pat) ->
              iterator.TI.pat iterator lb_pat;
          ) fields
      | Tpat_array _ -> model_append model ~loc "_ (pat array)" |> ignore
      | Tpat_lazy _ -> model_append model ~loc "_ (pat lazy)" |> ignore

      (* computation patterns *)
      | Tpat_value case -> ignore case
      | Tpat_exception _ -> model_append model ~loc "_ (pat exception)" |> ignore

      (* generic patterns *)
      | Tpat_or _ -> model_append model ~loc "_ (pat constructor)" |> ignore
    )
  in
  let expr ( iterator : TI.iterator ) ( item : expression ) =
    let { exp_desc; _ } = item in
    ( match exp_desc with
      | Texp_ident _ -> ()
      | Texp_constant _ -> ()
      | Texp_match _ -> ()
      | Texp_apply _ -> ()
      | Texp_function { cases; _ } ->
          List.iter
            ( fun { c_lhs; c_rhs; _ } ->
                parameter := true;
                iterator.TI.pat iterator c_lhs;
                parameter := false;
                iterator.TI.expr iterator c_rhs
            )
            cases;
      | _ -> super.TI.expr iterator item
    )
  in
  { super with
    TI.structure_item;
    module_binding; module_expr;
    value_binding;
    pat; expr;
    class_expr; class_structure; class_field
  }

class widget ~page () =
  let model = GTree.tree_store cols in
  let model_sort_default = GTree.model_sort model in
  let vbox = GPack.vbox () in

  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view = GTree.view ~model:model_sort_default ~headers_visible:false ~packing:sw#add ~width:350 ~height:500 () in

  let renderer_pixbuf = GTree.cell_renderer_pixbuf [`YPAD 0; `YALIGN 0.0; `XPAD 0] in
  let renderer_markup = GTree.cell_renderer_text [`YPAD 0] in
  let vc = GTree.view_column () in

  let _ = vc#pack ~expand:false renderer_pixbuf in
  let _  = vc#pack ~expand:false renderer_markup  in
  let _ = vc#add_attribute renderer_pixbuf "pixbuf" col_icon in
  let _ = vc#add_attribute renderer_markup "markup" col_markup in

  let _ = view#append_column vc in
  let _ = view#misc#set_property "enable-tree-lines" (`BOOL true) in
  let _ = view#set_tooltip_column col_tooltip.GTree.index in
  let buffer : Ocaml_text.buffer = page#buffer in
  let iterator = outline_iterator model in
  object (self)
    inherit GObj.widget vbox#as_widget

    val mutable selection_changed_signal = Obj.magic 0

    method load () =
      let compile_buffer () = page#compile_buffer ?join:(Some true) () in
      let cmt_opt = Binannot.read_cmt ~project:page#project ~filename:page#get_filename ~compile_buffer () in
      match cmt_opt with
      | None -> ()
      | Some (_, _, cmt) -> self#load_cmt cmt

    method private load_cmt cmt =
      view#selection#misc#handler_block selection_changed_signal ;
      model_clear model ;
      ( match cmt.cmt_annots with
        | Implementation structure -> iterator.TI.structure iterator structure
        | Partial_implementation items ->
            Array.iter ignore items
        | Interface signature -> iterator.TI.signature iterator signature
        | Partial_interface items ->
            Array.iter ignore items
        | Packed _ -> ()
      ) ;
      (* TODO: Remove this hack and set the bottom margin *)
      let loc = max_int in
      model_append model ~loc ~tooltip:"" "" |> ignore;

      view#selection#unselect_all ();
      view#selection#misc#handler_unblock selection_changed_signal

    method view = view

    method select_from_buffer ?(align : float option) (mark : Gtk.text_mark) = false

    method private selection_changed () =
      ( match view#selection#get_selected_rows with
        | [ path ] ->
            let row = model#get_iter path in
            let offset = model#get ~row ~column:col_loc in
            let text_iter = buffer#get_iter (`OFFSET offset) in
            let view : Text.view = page#view in
            view#scroll_lazy text_iter;
            buffer#select_range text_iter text_iter
        | _ -> ()
      )

    initializer
      selection_changed_signal <- view#selection#connect#changed ~callback:self#selection_changed

  end

let create = new widget
