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
module ColorOps = Color
open Preferences
open GUtil
open Cmt_format
open Location
open Typedtree
open! Asttypes
open! Types

module Log = Common.Log.Make(struct let prefix = "Outline_view" end)
let _ = Log.set_verbosity `DEBUG

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

let string_of_type_expr ?(is_method=false) te =
  match [@warning "-4"] Types.get_desc te with
  | Tarrow (lab, t1, t2, _) ->
      if is_method then Odoc_info.string_of_type_expr t2
      else begin
        let label =
          match lab with
          | Nolabel -> ""
          | Labelled l -> l ^ ":"
          | Optional l -> "?" ^ l ^ ":"
        in
        (* Still buggy: "?label:int" will print "?label:int option" *)
        sprintf "%s%s -> %s" label (Odoc_info.string_of_type_expr t1) (Odoc_info.string_of_type_expr t2)
      end
  | _ -> Odoc_info.string_of_type_expr te;;

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

class widget ~page () =
  let model = GTree.tree_store cols in
  let model_sort_default = GTree.model_sort model in
  let vbox = GPack.vbox () in

  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view = GTree.view ~model:model_sort_default ~headers_visible:false ~packing:sw#add ~width:350 ~height:500 () in

  let renderer_pixbuf = GTree.cell_renderer_pixbuf [`YPAD 0; `XPAD 0] in
  let vc = GTree.view_column () in

  let _ = vc#pack ~expand:false renderer_pixbuf in
  let _ = vc#add_attribute renderer_pixbuf "pixbuf" col_icon in

  let _ = view#append_column vc in
  object
    inherit GObj.widget vbox#as_widget

    method load () = ()

    method view = view

    method select_from_buffer ?(align : float option) (mark : Gtk.text_mark) = false

    method private append ?parent ?kind ?loc ?loc_body name typ =
      let markup = name ^ typ in
      let info = {
        typ ;
        kind ;
        location = loc ;
        body = loc_body ;
        mark = None
      } in
      let path, row =
        let row = model#append ?parent () in
        let path = model#get_path row in
        Option.iter (model#set ~row ~column:col_icon) (pixbuf_of_kind kind);
        path, row
      in
      ()


  end

let create = new widget

