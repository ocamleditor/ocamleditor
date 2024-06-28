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
open! Types

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
let col_lazy           : (unit -> unit) list GTree.column = cols#add Gobject.Data.caml
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

(** empty *)
let empty () =
  let pref = Preferences.preferences#get in
  let vp = GBin.viewport () in
  let label = GMisc.label ~xalign:0.5 ~yalign:0. ~xpad:3 ~ypad:3
      ~text:"Structure is not available" ~packing:vp#add () in
  vp#misc#modify_bg [`NORMAL, `NAME ?? (pref.outline_color_nor_bg)];
  label#misc#modify_fg [
    `NORMAL, `NAME ?? (pref.outline_color_nor_fg)
  ];
  vp#coerce;;

let dummy_re = Str.regexp ""

type point = { x : int; y : int }
let make_point x y = { x; y }

let { x = ox; y = oy } as origin = make_point 0 0

let count = ref 0
let parent_row = ref None

let model_clear (model : GTree.tree_store) =
  count := 0;
  parent_row := None;
  model#clear ()

let model_append
    ( model : GTree.tree_store )
    ?icon
    ( text : string )
  =
  let parent = !parent_row in
  let row = model#append ?parent () in
  let () = Option.iter (fun icon -> model#set ~row ~column:col_icon (??? icon)) icon in
  let () = model#set ~row ~column:col_markup text in
  let () = model#set ~row ~column:col_default_sort !count in
  incr count;
  row

let update_parent_markup
    ( model : GTree.tree_store )
    ( update_fn : string -> string )
  =
  match !parent_row with
  | Some row -> let markup = update_fn (model#get ~row ~column:col_markup) in
      model#set ~row ~column:col_markup markup
  | None -> ()

let b text = "<b>" ^ text ^ "</b>"
let i text = "<i>" ^ text ^ "</i>"

let string_of_id ?(default="") id =
  id |> Option.map Ident.name |> Option.value ~default

let string_of_functor_parameter = function
  | Typedtree.Unit -> "()"
  | Typedtree.Named (id, _, md_type) ->
      let md_name = string_of_id id in
      let { mty_type; _ } = md_type in
      let md_type = Odoc_info.string_of_module_type mty_type in
      "(" ^ md_name ^ " : TODO " ^ md_type ^ ")"

let outline_iterator (model : GTree.tree_store) =
  let super = Tast_iterator.default_iterator in
  let open! Tast_iterator in

  let append text = model_append model text |> ignore in
  let structure_item iterator (item : structure_item) =

    let { str_desc; _ } = item in
    ( match str_desc with
      | Tstr_eval (_, _) -> append "_ (eval)"
      | Tstr_value (is_rec, vb) ->
          if is_rec = Asttypes.Recursive then (
            let parent = !parent_row in
            parent_row :=  Some (model_append model (b "rec"));
            List.iter (iterator.value_binding iterator) vb;
            parent_row := parent
          )
          else
            List.iter (iterator.value_binding iterator) vb

      | Tstr_primitive _ -> append "_ (primitive)"
      | Tstr_type (_, _) -> append "_ (type)"
      | Tstr_typext _ -> append "_ (typext)"
      | Tstr_exception _ -> append "_ (exception)"
      | Tstr_module mb -> iterator.module_binding iterator mb
      | Tstr_recmodule _ -> append "_ (rec module)"
      | Tstr_modtype _ -> append "_ (modtype)"
      | Tstr_open _ -> append "_ (open)"
      | Tstr_class _ -> append "_ (class)"
      | Tstr_class_type _ -> append "_ (classtype)"
      | Tstr_include _ -> append "_ (include)"
      | Tstr_attribute _ -> append "_ (attribute)"
    )
  in

  let module_binding iterator (mb : module_binding) =
    let { mb_id; mb_expr; _ } = mb in
    let text = string_of_id mb_id in

    let parent = !parent_row in
    let row =  model_append model ~icon:Icons.module_impl (b text) in
    parent_row := Some row;
    iterator.module_expr iterator mb_expr;
    parent_row := parent
  in

  let module_expr
      ( iterator : Tast_iterator.iterator )
      { mod_desc; _ }
    =
    match mod_desc with
    | Tmod_ident (_, { txt; _ }) ->
        let alias = String.concat "." @@ Longident.flatten txt in
        model_append model ~icon:Icons.simple (i alias) |> ignore
    | Tmod_structure structure ->
        iterator.structure iterator structure;
    | Tmod_functor (p, expr) ->
        let param = string_of_functor_parameter p in
        update_parent_markup model (fun name -> name ^ " " ^ param) ;
        iterator.module_expr iterator expr;
    | Tmod_apply (m1, m2, _) ->
        iterator.module_expr iterator m1;
        iterator.module_expr iterator m2;
    | Tmod_constraint (_, _, _, _) ->
        model_append model ~icon:Icons.simple "Constraint" |> ignore;
    | Tmod_unpack (_, _) ->
        model_append model ~icon:Icons.simple "Unpack What" |> ignore
  in

  let value_binding
      ( iterator : Tast_iterator.iterator )
      { vb_pat; vb_expr; _ }
    =
    let { pat_desc; _ } = vb_pat in
    let { exp_type; exp_loc; _ } = vb_expr in
    ( match pat_desc with
      | Tpat_any -> ()  (* let _ = .. *)
      | Tpat_constant _ -> () (* let () = .. *)
      | Tpat_var (id, _) ->
          let icon = if is_function exp_type then Icons.func else Icons.simple in
          let st = Odoc_info.string_of_type_expr exp_type in
          model_append model ~icon (Ident.name id ^ " : " ^ st) |> ignore;

      | _ -> model_append model "_ (value)" |> ignore;
    )
  in
  { super with
    structure_item;
    module_binding; module_expr;
    value_binding
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
  let iterator = outline_iterator model in
  object (self)
    inherit GObj.widget vbox#as_widget

    method load () =
      let compile_buffer () = page#compile_buffer ?join:(Some true) () in
      let cmt_opt = Binannot.read_cmt ~project:page#project ~filename:page#get_filename ~compile_buffer () in
      match cmt_opt with
      | None -> ()
      | Some (_, _, cmt) -> self#load_cmt cmt


    method private load_cmt cmt =
      model_clear model ;
      match cmt.cmt_annots with
      | Implementation impl -> iterator.structure iterator impl
      | _ ->
          print_endline "Not an implementation"

    method view = view

    method select_from_buffer ?(align : float option) (mark : Gtk.text_mark) = false


  end

let create = new widget
