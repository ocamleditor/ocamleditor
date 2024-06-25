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
open GUtil
open Cmt_format
open Typedtree
open! Asttypes
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


type kind =
  | Function
  | Simple
  | Class
  | Class_virtual
  | Class_type
  | Class_inherit
  | Class_let_bindings
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
  | Type_open
  | Module
  | Module_functor
  | Module_type
  | Module_include
  | Exception
  | Error
  | Warning
  | Folder_warnings
  | Folder_errors
  | Dependencies
  | Bookmark of GdkPixbuf.pixbuf
  | Unknown

let pixbuf_of_kind = function
  | Function -> Some (??? Icons.func)
  | Simple -> Some (??? Icons.simple)
  | Method -> Some (??? Icons.met)
  | Method_private -> Some (??? Icons.met_private)
  | Method_virtual -> Some (??? Icons.met_virtual)
  | Method_private_virtual -> Some (??? Icons.met_private_virtual)
  | Method_inherited -> Some (??? Icons.met)
  | Initializer -> Some (??? Icons.init)
  | Attribute -> Some (??? Icons.attribute)
  | Attribute_mutable -> Some (??? Icons.attribute_mutable)
  | Attribute_mutable_virtual -> Some (??? Icons.attribute_mutable_virtual)
  | Attribute_virtual -> Some (??? Icons.attribute_virtual)
  | Type -> Some (??? Icons.typ)
  | Type_abstract -> Some (??? Icons.type_abstract)
  | Type_variant -> Some (??? Icons.type_variant)
  | Type_record -> Some (??? Icons.type_record)
  | Type_open -> Some (??? Icons.type_variant)
  | Class -> Some (??? Icons.classe)
  | Class_virtual -> Some (??? Icons.class_virtual)
  | Class_type -> Some (??? Icons.class_type)
  | Class_inherit -> Some (??? Icons.class_inherit)
  | Class_let_bindings -> None
  | Module -> Some (??? Icons.module_impl)
  | Module_functor -> Some (??? Icons.module_funct)
  | Module_type -> Some (??? Icons.module_type)
  | Module_include -> Some (??? Icons.module_include)
  | Exception -> Some (??? Icons.exc)
  | Error -> Some (??? Icons.error_14)
  | Warning -> Some (??? Icons.warning_14)
  | Folder_warnings -> Some (??? Icons.folder_warning)
  | Folder_errors -> Some (??? Icons.folder_error)
  | Dependencies -> None
  | Bookmark pixbuf -> Some pixbuf
  | Unknown -> None;;

let pixbuf_of_kind kind_opt = Option.bind kind_opt pixbuf_of_kind

type info = {
  typ          : string;
  kind         : kind option;
  location     : Location.t option;
  body         : Location.t option;
  mutable mark : Gtk.text_mark option;
}

let cols               = new GTree.column_list
let col_icon           = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf")
let col_name           = cols#add Gobject.Data.string
let col_markup         = cols#add Gobject.Data.string
let col_lazy           : (unit -> unit) list GTree.column = cols#add Gobject.Data.caml
let col_default_sort   = cols#add Gobject.Data.int

let string_rev str =
  let len = String.length str in
  Bytes.init len (fun i -> str.[len - i - 1])
  |> Bytes.to_string

let string_rev = Miscellanea.Memo.create string_rev;;

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

module M = struct
  type t = int
  type s = string
  let t = 1
  module N = struct
    let v = 2
  end
end

let count = ref 0
let parent_row = ref None

let model_clear (model : GTree.tree_store) =
  count := 0;
  parent_row := None;
  model#clear ()

let model_append (model : GTree.tree_store) ?icon text =
  let parent = !parent_row in
  let row = model#append ?parent () in
  let () = Option.iter (fun icon -> model#set ~row ~column:col_icon (??? icon)) icon in
  let () = model#set ~row ~column:col_markup text in
  let () = model#set ~row ~column:col_default_sort !count in
  incr count;
  row

let update_parent_markup
    ( model : GTree.tree_store )
    ( fn : string -> string)
  =
  match !parent_row with
  | None -> ()
  | Some row -> let markup = fn (model#get ~row ~column:col_markup) in
      model#set ~row ~column:col_markup markup

let b text = "<b>" ^ text ^ "</b>"
let i text = "<i>" ^ text ^ "</i>"

let string_of_id ?(default="") id =
  id |> Option.map Ident.name |> Option.value ~default

let string_of_functor_parameter = function
  | Typedtree.Unit -> "()"
  | Typedtree.Named (id, _, _) ->
      let name = string_of_id id in
      "(" ^ name ^ " : type)"

let outline_iterator (model : GTree.tree_store) =
  let super = Tast_iterator.default_iterator in
  let open! Tast_iterator in

  let append text = model_append model text |> ignore in
  let structure_item iterator (item : structure_item) =

    let { str_desc; _ } = item in
    ( match str_desc with
      | Tstr_eval (_, _) -> append "_ (eval)"
      | Tstr_value (_, _) -> append "_ value)"
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

  let module_expr iterator { mod_desc; _ } =
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
        model_append model ~icon:Icons.simple "Unpack ???" |> ignore

  in
  { super with structure_item; module_binding; module_expr }

class widget ~page () =
  let model = GTree.tree_store cols in
  let model_sort_default = GTree.model_sort model in
  let vbox = GPack.vbox () in

  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view = GTree.view ~model:model_sort_default ~headers_visible:false ~packing:sw#add ~width:350 ~height:500 () in

  let renderer_pixbuf = GTree.cell_renderer_pixbuf [`YPAD 0; `XPAD 0] in
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
