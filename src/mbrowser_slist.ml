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
open GdkKeysyms
open Miscellanea
open Oe

type title = {
  title     : string;
  subtitle  : string;
  tooltip   : string;
}

(** pixbuf_of_kind *)
let pixbuf_of_kind = function
  | Pvalue                  -> Icons.func
  | Pfunc                   -> Icons.func
  | Pattribute              -> Icons.empty_14
  | Pmethod                 -> Icons.met
  | Pmethod_private_virtual -> Icons.met_private_virtual
  | Pmethod_private         -> Icons.met_private
  | Pmethod_virtual         -> Icons.met_virtual
  | Ptype                   -> Icons.typ
  | Plabel                  -> Icons.empty_14
  | Pconstructor            -> Icons.empty_14 (*Icons.constructor*)
  | Pexception              -> Icons.exc
  | Pmodule                 -> Icons.module_impl
  | Pmodtype                -> Icons.module_impl
  | Pclass                  -> Icons.classe
  | Pcltype                 -> Icons.class_type
  | Ptype_abstract          -> Icons.type_abstract
  | Ptype_variant           -> Icons.type_variant
  | Ptype_record            -> Icons.type_record
  | Std_lib                 -> Icons.empty_14
  | Lib                     -> Icons.empty_14
;;

module Index = struct
  let len = 256
  let create () = Array.make len []
  let clear ind = for i = 0 to len - 1 do Array.unsafe_set ind i [] done

  let add ind name path =
    if String.length name > 0 then
      let i = Char.code (Char.lowercase_ascii name.[0]) in
      Array.unsafe_set ind i
        ((*List.sort (fun (_, a) (_, b) -> compare a b)*) ((path, name) :: (Array.unsafe_get ind i)))

  let find_all ind name =
    if String.length name > 0 then
      let i = Char.code (Char.lowercase_ascii name.[0]) in
      Array.unsafe_get ind i
    else []

  let close index =
    for i = 0 to len - 1 do
      Array.unsafe_set index i (List.rev (Array.unsafe_get index i))
    done

end

let cols            = new GTree.column_list
let col_icon        = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf")
let col_search      = cols#add Gobject.Data.string
let col_type_descr  = cols#add Gobject.Data.string
let col_add_descr   = cols#add Gobject.Data.string
let col_symbol_data = cols#add Gobject.Data.caml

(** symbol_list *)
class symbol_list ~kind ?(is_completion=false) ?model ?(index=Index.create ()) ?packing ?width ?height () =
  let renderer        = GTree.cell_renderer_text [`YPAD (match kind with `Search -> 5 | _ -> 0)] in
  let renderer_pixbuf = GTree.cell_renderer_pixbuf [] in
  let model           = match model with Some m -> m | _ -> GTree.list_store cols in
  let vc_icon         = GTree.view_column ~renderer:(renderer_pixbuf, ["pixbuf", col_icon]) ~title:"" () in
  let vc_type_descr   = GTree.view_column ~renderer:(renderer, ["markup", col_type_descr]) ~title:"Type Description" () in
  let vc_add_descr    = GTree.view_column ~renderer:(renderer, ["markup", col_add_descr]) ~title:"" () in
  let sw              = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ?packing () in
  let view            = GTree.view
    ?width ?height
    ~model:model
    ~enable_search:false
    ~headers_visible:true
    ~reorderable:false
    ~packing:sw#add ()
  in
  let _               = view#append_column vc_icon in
  let _               = view#append_column vc_type_descr in
  let _               = view#append_column vc_add_descr in
  let _               = view#set_headers_visible false in
  let _               = vc_add_descr#set_visible false in
  let _               = view#set_search_column col_search.GTree.index in
  (*let _               = view#misc#set_property "enable-grid-lines" (`INT 1) in*)

object (self)
  inherit GObj.widget sw#as_widget
  val backspace = new backspace ()
  val mutable title = {title=""; subtitle=""; tooltip=""}
  val mutable is_search_output = false
  val mutable ts_changed = 0.0

  method connect = new symbol_list_signals ~backspace
  method disconnect = new symbol_list_signals_disconnector ~backspace
  method model = model
  method view = view
  method index = index
  method vc_icon = vc_icon
  method vc_type_descr = vc_type_descr
  method vc_add_descr = vc_add_descr
  method sw = sw
  method renderer = renderer
  method renderer_pixbuf = renderer_pixbuf
  method title = title
  method set_title ~subtitle ?(tooltip="") text = title <- {title=text; subtitle=subtitle; tooltip=tooltip}
  method is_search_output = is_search_output
  method set_is_search_output x = is_search_output <- x
  method ts_changed = ts_changed
  method kind : [ `Directory | `Library| `Module | `Class | `Search | `Index ] = kind

  val mutable length = 0

  initializer
    view#misc#modify_font_by_name Preferences.preferences#get.Preferences.pref_compl_font;
    ignore (view#event#connect#key_press ~callback:begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
      if key = _BackSpace then begin
        backspace#call();
        true
      end else false
    end);
    ignore (view#selection#connect#changed ~callback:begin fun () ->
      ts_changed <- Unix.gettimeofday();
      (*try
        let path = List.hd view#selection#get_selected_rows in
        view#scroll_to_cell ~align:(0.38, 0.) path vc_icon;
      with Failure "hd" -> ()*)
    end);

  method length = length

  method fill symbols =
    length <- 0;
    model#clear();
    Index.clear index;
    (* Fill model *)
    let markup =
      match kind with
        | `Search when not is_completion ->
          sprintf "<span weight='bold' color='%s' bgcolor='#ffffff'>%s</span>\n%s" Oe_config.module_browser_secondary_title_color
        | _ ->
          sprintf "<span weight='bold' color='%s'>%s</span>\n%s" Oe_config.module_browser_secondary_title_color
    in
    List.iter begin fun symbol ->
      let name = Symbols.get_name symbol in
      let symbol_path = Symbols.concat_value_path symbol in
      let row = model#append () in
      model#set ~row ~column:col_symbol_data symbol;
      model#set ~row ~column:col_icon (pixbuf_of_kind symbol.sy_kind);
      model#set ~row ~column:col_search name;
      let local = symbol.sy_local in
      let descr = if local then sprintf "%s : %s" name symbol.sy_type else symbol.sy_type in
      let name_escaped = Glib.Markup.escape_text name in
      let descr = Print_type.markup2 descr in
      let descr = replace_first ["\\([ \t]*\\)\\("^(Str.quote name_escaped)^"\\)\\([ \t]+\\|$\\)", "\\1<b>\\2</b>\\3"] descr in
      let descr =
        let lines = Str.split (!~ "\n") descr in
        if List.length lines > 4 then (sprintf "<b>%s</b> (...)" name_escaped)
        else descr
      in
      let descr =
        if local then sprintf "<span color='#0000ff'>%s</span>" descr else descr
      in
      let descr =
        if is_search_output && (symbol.sy_kind <> Pmodule || symbol_path <> name)
        then markup (Glib.Markup.escape_text symbol_path) descr else descr
      in
      model#set ~row ~column:col_type_descr descr;
      let path = model#get_path row in
      Index.add index name path;
      length <- length + 1;
    end symbols;
    Index.close index;

end

and symbol_list_signals ~backspace = object
  inherit GUtil.ml_signals [backspace#disconnect]
  method backspace = backspace#connect ~after
end
and symbol_list_signals_disconnector ~backspace = object
  method backspace = backspace#disconnect
end

and backspace () = object (self) inherit [unit] GUtil.signal () as super end
