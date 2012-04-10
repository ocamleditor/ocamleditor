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

open Printf
open Code_insight
open Miscellanea
open GdkKeysyms

let pixbuf_of_kind = function
  | Pvalue       -> Icons.func
  | Pfunc        -> Icons.func
  | Pmethod      -> Icons.met
  | Pmethod_private_virtual -> Icons.met_private_virtual
  | Pmethod_private -> Icons.met_private
  | Pmethod_virtual -> Icons.met_virtual
  | Ptype        -> Icons.typ
  | Plabel       -> Icons.none_14
  | Pconstructor -> Icons.constructor
  | Pexception   -> Icons.exc
  | Pmodule      -> Icons.module_impl
  | Pmodtype     -> Icons.module_impl
  | Pclass       -> Icons.classe
  | Pcltype      -> Icons.class_type
;;

(** widget *)
class widget ?packing ?width ?height () =
  let renderer        = GTree.cell_renderer_text [] in
  let renderer_pixbuf = GTree.cell_renderer_pixbuf [] in
  let cols            = new GTree.column_list in
  let col_icon        = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf") in
  let col_modlid      = cols#add Gobject.Data.string in
  let col_name        = cols#add Gobject.Data.string in
  let col_descr       = cols#add Gobject.Data.string in
  let col_longid      = cols#add Gobject.Data.string in
  let col_data        = cols#add Gobject.Data.caml in
  let col_path        = cols#add Gobject.Data.string in
  let model           = GTree.list_store cols in
  let vc_icon         = GTree.view_column ~renderer:(renderer_pixbuf, ["pixbuf", col_icon]) ~title:"Kind" () in
  let vc_modlid       = GTree.view_column ~renderer:(renderer, ["text", col_modlid]) ~title:"Module" () in
  let vc_name         = GTree.view_column ~renderer:(renderer, ["text", col_name]) ~title:"Symbol" () in
  let vc_descr        = GTree.view_column ~renderer:(renderer, ["markup", col_descr]) ~title:"Description" () in
  let vc_longid       = GTree.view_column ~renderer:(renderer, ["text", col_longid]) ~title:"Longid" () in
  let sw              = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ?packing () in
  let view            = GTree.view
    ~model:model
    ~enable_search:false
    ~headers_visible:true
    ~reorderable:false
    ?width
    ?height
    ~packing:sw#add ()
  in
  let _ = view#append_column vc_icon in
  let _ = view#append_column vc_modlid in
  let _ = view#append_column vc_name in
  let _ = view#append_column vc_descr in
  let _ = view#append_column vc_longid in
  let _ = view#set_headers_visible false in
  let _ = vc_name#set_visible false in
object (self)
  inherit GObj.widget sw#as_widget
  val backspace = new backspace ()
  method connect = new widget_signals ~backspace
  method model = model
  method col_icon : [`pixbuf] Gobject.obj GTree.column = col_icon
  method col_modlid : string GTree.column = col_modlid
  method col_name : string GTree.column = col_name
  method col_descr : string GTree.column = col_descr
  method col_longid : string GTree.column = col_longid
  method col_data : (Code_insight.t * Code_insight.item) GTree.column = col_data
  method col_path : string GTree.column = col_path
  method vc_icon = vc_icon
  method vc_modlid = vc_modlid
  method vc_name = vc_name
  method vc_descr = vc_descr
  method view = view
  method sw = sw
  method renderer = renderer
  method renderer_pixbuf = renderer_pixbuf

  initializer
    ignore (view#event#connect#key_press ~callback:begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
      if key = _BackSpace then begin
        backspace#call();
        true
      end else false
    end);

  method fill insight =
    model#clear();
    (** Fill model *)
    List.iter begin fun ({ci_modlid=modlid; ci_name=name; ci_longid=longid; ci_descr=descr; ci_kind=kind; ci_cdecl=cdecl; ci_local=local} as data) ->
      let row = model#append () in
      model#set ~row ~column:col_icon (pixbuf_of_kind kind);
      model#set ~row ~column:col_modlid modlid;
      model#set ~row ~column:col_name name;
      model#set ~row ~column:col_longid (String.concat "." (List.rev longid));
      let descr = if local then sprintf "%s : %s" name descr else descr in
      let descr = Print_type.markup2 descr in
      let descr = replace_first ["\\([ \t]*\\)\\("^(Str.quote (Glib.Markup.escape_text name))^"\\)\\([ \t]+\\|$\\)", "\\1<b>\\2</b>\\3"] descr in
      let descr =
        let lines = Str.split (!~ "\n") descr in
        if List.length lines > 4 then (sprintf "<b>%s</b>..." (Glib.Markup.escape_text name))
        else if List.mem kind [Pmodule; Pmodtype] && modlid = ""
        then (sprintf "<b>%s</b> <i><small>%s</small></i>" (Glib.Markup.escape_text name)
          (match descr with "" -> "" | x -> "&lt;" ^ x ^ "&gt;"))
        else if List.mem kind [Pmodule; Pmodtype; Pclass] then (sprintf "<b>%s</b>" (Glib.Markup.escape_text name))
        else descr
      in
      model#set ~row ~column:col_data (insight, data);
      model#set ~row ~column:col_descr descr;
    end insight.ci_result;

end

and widget_signals ~backspace = object
  inherit GUtil.ml_signals [backspace#disconnect]
  method backspace = backspace#connect ~after
end

and backspace () = object (self) inherit [unit] GUtil.signal () as super end


















