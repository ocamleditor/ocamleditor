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

open Types
open Longident
open Miscellanea
open Printf
open Oe_config
open Outcometree

type pkind =
    Pvalue
  | Ptype
  | Plabel
  | Pconstructor
  | Pmodule
  | Pmodtype
  | Pclass
  | Pcltype

let string_of_kind = function
    Pvalue -> "val"
  | Ptype -> "type"
  | Plabel -> "label"
  | Pconstructor -> "constructor"
  | Pmodule -> "module"
  | Pmodtype -> "signature"
  | Pclass -> "class &#8594;"
  | Pcltype -> "class type"

let env = ref (Env.open_pers_signature "Pervasives" Env.initial)

let find_path modlid = Misc.find_in_path_uncap !Config.load_path (modlid^(".cmi"))

let list_modules ~path =
  List.sort (Pervasives.compare) (List.fold_left
    begin fun modules dir ->
      let l = List.filter (fun x -> Filename.check_suffix x ".cmi")
        (Array.to_list (try Sys.readdir dir with (Sys_error _) as ex -> [||])) in
      let l = List.map
        begin fun x ->
          String.capitalize (Filename.chop_suffix x ".cmi")
        end l in
      List.fold_left begin fun modules item ->
        if List.mem item modules then modules else item :: modules
      end modules l
    end [] path)


(** parse_class_declaration *)
let parse_class_declaration modlid id cd =
  let buf = Buffer.create 1024 in
  let formatter = Format.formatter_of_buffer buf in
  let print id printtyp typ =
    Buffer.clear buf;
    printtyp typ;
    Format.pp_print_flush formatter ();
    let desc = Buffer.contents buf in
    (Buffer.contents buf)
  in
  let dummy_id = Ident.create "" in
  match cd with {
    cty_params = cty_params;
    cty_type = cty_type;
    cty_path = cty_path
  } ->
    begin
      match Printtyp.tree_of_class_declaration id cd Types.Trec_first with
        | Osig_class (vir_flag, name, params, clt, rs) ->
          let rec parse_class_type = function
            | Octy_signature (self_ty, csil) ->
              List.map begin function
                | Ocsg_method (name, priv, virt, ty) ->
                  (sprintf "<b>%s</b>" name), (print dummy_id (!Oprint.out_type formatter) ty);
                | Ocsg_value (name, virt, priv, ty) ->
                  (sprintf "val <b>%s</b>%s%s" name (if virt then "virtual " else "") (if priv then "private " else "")),
                  (print dummy_id (!Oprint.out_type formatter) ty);
                | _ -> "*", "***"
              end csil
            | Octy_fun (_, _, clt) -> parse_class_type clt
            | _ -> []
          in parse_class_type clt
        | _ -> []
    end


(** signature' *)
let rec signature' (path, modlid) =
  try
    let sign = Env.read_signature modlid path in
    let buf = Buffer.create 1024 in
    let formatter = Format.formatter_of_buffer buf in
    let print ?cdecl kind id printtyp typ =
      Buffer.clear buf;
      printtyp typ;
      Format.pp_print_flush formatter ();
      let desc = Buffer.contents buf in
      Buffer.clear buf;
      Printtyp.ident formatter id;
      Format.pp_print_flush formatter ();
      Buffer.contents buf, modlid, desc, kind, cdecl
    in
    let result = List.fold_left begin fun acc item ->
      match item with
        | Tsig_class (id, class_declaration, _) ->
          begin
            parse_class_declaration modlid id class_declaration;
            let class_name = print ?cdecl:(Some class_declaration) Pclass id (Printtyp.ident formatter) id in
            class_name ::
(*            (print Pclass id (Printtyp.class_declaration id formatter) class_declaration) ::*)
            acc;
          end
(*            (print Pclass id (Printtyp.ident formatter) id) :: acc;*)
        | Tsig_cltype (id, cltype_declaration, _) -> acc
        | Tsig_value (id, value_description) ->
          (print Pvalue id (Printtyp.value_description id formatter)
            value_description) :: acc;
        | Tsig_type (id, type_declaration, _) ->
          let acc = begin match type_declaration.type_manifest with
            | None ->
              (print Ptype id (Printtyp.type_declaration id formatter)
                type_declaration) :: acc
            | Some te -> begin match te.desc with
              | Tobject (te, me) -> acc (* Niente definizione dei tipi oggetto *)
              | _ -> (print Ptype id (Printtyp.type_declaration id formatter)
                type_declaration) :: acc
            end
          end in (* Costruttori dei tipi varianti *)
          begin match type_declaration.type_kind with
            | Type_variant cc ->
              List.fold_left begin fun acc (n, tel) ->
                let n, m, d, k, cdecl =
                  if List.length tel = 0 then
                    print Pconstructor (Ident.create n) ignore ()
                  else begin
                    let tel = List.map
                      (print Pconstructor (Ident.create n) (Printtyp.type_expr formatter))
                      tel in
                    List.fold_left begin fun (_, _, d', _, _) (n, m, d, k, cdecl) ->
                      (n, m, d'^" * "^d, k, cdecl)
                    end ("", "", "", Pconstructor, None) tel
                  end in
                  let d = replace_first ["^ \\* ", ""] d in
                  let d = n^(if String.length d > 0 then " of "^d else "") in
                  let d = d ^ " : " ^ (Ident.name id) in
                  (n, m, d, k, cdecl) :: acc
              end acc cc

            | _ -> acc
          end
        | Tsig_exception (id, exception_declaration) ->
          (print Pconstructor id (Printtyp.exception_declaration id formatter)
            exception_declaration) :: acc;
        | Tsig_module (id, module_type, _) ->
          (print Pmodule id (Printtyp.ident formatter) id) :: acc;
        | Tsig_modtype (id, modtype_declaration) ->
          (print Pmodtype id (Printtyp.ident formatter) id) :: acc;
    end [] sign in
    let result = List.sort begin fun (n1, _, _, _, _) (n2, _, _, _, _) ->
      String.compare (String.lowercase n1) (String.lowercase n2)
    end result in
    result, Unix.gettimeofday()
  with (Env.Error e as exc) ->
    begin
      Env.report_error Format.std_formatter e;
      print_newline();
      match e with
        | Env.Inconsistent_import _ ->
          Env.reset_cache();
          signature' (path, modlid);
        | _ -> raise exc
    end
  | Not_found -> [], Unix.gettimeofday()

let fsignature = (Memo.fast ~f:signature')

let signature ?path modlid =
  try
    let path = match path with None -> find_path modlid | Some p -> p in
    fst (fsignature
      ~force:(fun (_, ts) -> (Unix.stat path).Unix.st_mtime > ts)
      (path, modlid)
    )
  with Not_found -> []


(* Popup widget *)

open Gobject.Data
open GdkKeysyms
open GTree

(** class_insight *)
class class_insight ~modlid ~clsid ~class_declaration ?packing () =
  let cols = new GTree.column_list in
  let col_meth = cols#add Gobject.Data.string in
  let col_meth_type = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let renderer = GTree.cell_renderer_text
    [`XALIGN 0.; (*`YPAD 3;*) `CELL_BACKGROUND !Preferences.preferences.Preferences.pref_bg_color_popup] in
  let vc_meth = GTree.view_column ~title:"" ~renderer:(renderer, ["markup", col_meth]) () in
  let vc_meth_type = GTree.view_column ~title:"" ~renderer:(renderer, ["markup", col_meth_type]) () in
  let view = GTree.view ~model ~headers_visible:false ?packing () in
  let _ = view#misc#modify_base [`NORMAL, `NAME !Preferences.preferences.Preferences.pref_bg_color_popup] in
  let _ = view#misc#modify_font_by_name !Preferences.preferences.Preferences.pref_compl_font in
  let _ = view#misc#modify_text [`NORMAL, `NAME !Preferences.preferences.Preferences.pref_fg_color_popup ] in
  let _ = view#append_column vc_meth in
  let _ = view#append_column vc_meth_type in
  let cdecl = parse_class_declaration modlid (Ident.create clsid) class_declaration in
  let _ =
    List.iter begin fun (name, typ) ->
      let row = model#append() in
      model#set ~row ~column:col_meth name;
      model#set ~row ~column:col_meth_type (Print_type.markup2 ((*Glib.Markup.escape_text*) typ));
    end cdecl
  in
  object (self)
    inherit GObj.widget_full view#as_widget as super
    method title = sprintf "Class <tt>%s.%s</tt>" modlid clsid
    method event = view#event
  end


(** popup *)
class popup ?modlid ~on_row_activated ~on_type ~on_search ~possibilities
  ?(show_module=false) () =
  (* Markup types *)
  let possibilities = List.map (fun (n, m, d, k, cdecl) ->
    n, m, (Print_type.markup2 d), (string_of_kind k), cdecl) possibilities in
  (* Font for names *)
  let possibilities =
    List.map begin fun (n, m, d, k, cdecl) ->
      n, m, (replace_first
        ["\\([ \t]*\\)\\("^(Str.quote (Glib.Markup.escape_text n))^"\\)\\([ \t]+\\|$\\)", "\\1<b>\\2</b>\\3"] d), k, cdecl
    end possibilities
  in
  let height = max (min (List.length possibilities * (15 + 8)) 400) 150 in
  let w = if false && Oe_config.is_win32 then
    GWindow.window
      ~kind:`POPUP
      ~type_hint:`MENU
      ~show:false
      ~border_width:1
      ~modal:true ()
    else
      GWindow.window
        ~decorated:false
        ~show:false
        ~border_width:1
        ~modal:false ()
  in
  let _ = w#set_skip_pager_hint true in
  let _ = w#set_skip_taskbar_hint true in
  let ebox = GBin.event_box ~packing:w#add () in
  let box = GPack.vbox ~border_width:0 ~packing:ebox#add () in
  let title = GMisc.label ~markup:"" ~xalign:0.5 ~xpad:3 ~ypad:3 ~packing:(box#pack ~expand:false) () in
  let _ = title#set_use_markup true in
  (*  *)
  let sw = GBin.scrolled_window
    ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC
    ~border_width:0 ~shadow_type:`NONE
    ~packing:box#add () in
  let sw2 = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:box#add ~show:false () in
  let cols = new GTree.column_list in
  let c_name = cols#add string in
  let c_module = cols#add string in
  let c_desc = cols#add string in
  let c_kind = cols#add string in
  let c_cdecl = cols#add caml in
  let model = GTree.list_store cols in
  let listview = GTree.view ~border_width:0 ~width:700 ~height ~model ~packing:sw#add
    ~enable_search:false ~search_column:0 ~headers_visible:false () in
  let _ = listview#misc#set_property "enable-grid-lines" (`INT 0) in
  let v_name = GTree.view_column
    ~renderer:(GTree.cell_renderer_text
      [`XALIGN 0.; `CELL_BACKGROUND !Preferences.preferences.Preferences.pref_bg_color_popup],
      ["text", c_name]) () in
  let v_module = GTree.view_column
    ~renderer:(GTree.cell_renderer_text
      [`XALIGN 0.; `CELL_BACKGROUND !Preferences.preferences.Preferences.pref_bg_color_popup],
      ["text", c_module]) () in
  let v_desc = GTree.view_column
    ~renderer:(GTree.cell_renderer_text
      [`XALIGN 0.; `CELL_BACKGROUND !Preferences.preferences.Preferences.pref_bg_color_popup],
      ["markup", c_desc]) () in
  let v_kind = GTree.view_column
    ~renderer:(GTree.cell_renderer_text
      [`XALIGN 1.; `CELL_BACKGROUND !Preferences.preferences.Preferences.pref_bg_color_popup],
      ["markup", c_kind]) () in
  let _ = ebox#misc#modify_bg [`NORMAL, `NAME !Preferences.preferences.Preferences.pref_bg_color_popup] in
  let _ = listview#misc#modify_base [`NORMAL, `NAME !Preferences.preferences.Preferences.pref_bg_color_popup] in
  let _ = listview#misc#modify_font_by_name !Preferences.preferences.Preferences.pref_compl_font in
  let _ = listview#misc#modify_text [`NORMAL, `NAME !Preferences.preferences.Preferences.pref_fg_color_popup ] in
  let color = Color.set_value 0.80 (`NAME !Preferences.preferences.Preferences.pref_bg_color_popup) in
  let _ = w#misc#modify_bg [`NORMAL, color] in
  object (self)
    inherit GObj.widget w#as_widget as super
    method private do_search txt =
      try
        let row = model#get_iter (Path.create [0]) in
        let found = ref false in
        let pat = Str.regexp_string_case_fold txt in
        let search' txt =
          let name = model#get ~row ~column:c_name in
          if Str.string_match pat name 0 then found := true
        in
        search' txt;
        while not !found && model#iter_next row do search' txt done;
        if !found then begin
          listview#set_cursor (model#get_path row) v_desc;
          listview#selection#select_iter row;
        end;
      with Failure "GtkTree.TreeModel.get_iter" -> ()
    method search txt =
      self#do_search txt;
      self#present();
    method private row_text path = model#get ~row:(model#get_iter path) ~column:c_name
    method private window = w
    method present () =
      listview#scroll_to_cell ~align:(0.5, 0.5) (self#get_current_path()) v_desc;
      w#present()
    method private get_current_path () =
      try List.hd (listview#selection#get_selected_rows) with _ -> Path.create [0]
    method activate () = listview#row_activated (self#get_current_path()) v_name;
    method event = w#event
    method move = w#move
    method window = w
    method set_title label =
      if label = "" then (title#misc#hide()) else (title#misc#show());
      w#set_title label;
      kprintf title#set_label "<span color=\"%s\" weight=\"bold\" underline=\"none\" font_size=\"x-large\"> %s </span>"
        !Preferences.preferences.Preferences.pref_fg_color_popup label;
      v_desc#set_visible true;
      v_desc#set_widget (Some (GMisc.label
        ~markup:(sprintf "<span color=\"%s\" weight=\"bold\" size=\"large\">%s</span>"
          !Preferences.preferences.Preferences.pref_fg_color_popup label) ())#coerce);
    initializer
      listview#set_rules_hint false;
      if show_module then ignore(listview#append_column v_module);
      listview#append_column v_desc;
      listview#append_column v_kind;
      listview#connect#row_activated ~callback:begin fun path _ ->
        on_row_activated (self#row_text path);
        w#destroy()
      end;
      (* Contenuto *)
      List.iter begin fun (n, m, d, k, cdecl) ->
        let n = Convert.to_utf8 n in
        let m = Convert.to_utf8 m in
        let d = Convert.to_utf8 d in
        let k = Convert.to_utf8 k in
        let row = model#append () in
        model#set ~row ~column:c_name n;
        if show_module then ignore (model#set ~row ~column:c_module m);
        model#set ~row ~column:c_desc d;
        model#set ~row ~column:c_kind k;
        model#set ~row ~column:c_cdecl cdecl;
      end possibilities;
      (* Eventi sulla finestra *)
      w#event#connect#focus_out ~callback:begin fun ev ->
        w#destroy();
        true
      end;
      ignore (w#event#connect#key_release ~callback:begin fun ev ->
        let state = GdkEvent.Key.state ev and key = GdkEvent.Key.keyval ev in
        if key = _Escape then (w#destroy(); true)
        else if key = _Right then begin
          match listview#selection#get_selected_rows with
            | path :: _ ->
              let row = model#get_iter path in
              if (model#get ~row ~column:c_cdecl) <> None then begin
                sw#misc#hide();
                let clsid = model#get ~row ~column:c_name in
                let cdecl = match model#get ~row ~column:c_cdecl with None -> assert false | Some cd -> cd in
                let modlid = match modlid with Some x -> x | _ -> assert false in
                let cli = new class_insight ~modlid ~clsid ~class_declaration:cdecl ~packing:sw2#add () in
                self#set_title cli#title;
                cli#event#connect#key_release ~callback:begin fun ev ->
                  let keyval = GdkEvent.Key.keyval ev in
                  if List.mem keyval [_Escape; _Left] then begin
                    cli#destroy();
                    sw2#misc#hide();
                    sw#misc#show();
                    true
                  end else false;
                end;
                sw2#misc#show();
                true
              end else false
            | _ -> false
        end else if not (List.mem key [
          _Return;
          _Up;
          _Down;
          _Page_Up;
          _Page_Down;
          _End;
          _Home;
          _Escape
        ]) then begin
          self#search (on_search());
          false
        end
        else false;
      end);
      (* Ricerca iniziale: posizionamento carattere per carattere *)
      let search = on_search() in
      let s = ref "" in
      for i = 0 to String.length search - 1 do
        s := !s ^ (String.sub search i 1);
        self#do_search !s;
      done;
      listview#misc#grab_focus()
  end



(* Autocompletamento per uident (Moduli e costruttori) *)

let uident_lookup ~paths_opened ~on_row_activated ~on_type ~on_search =
  let modules = List.map (fun m -> m, "", m, Pmodule, None)
    (list_modules ~path:!Config.load_path) in

  let possibilities = (try


  let possibilities = List.fold_left begin fun sign modlid ->
    (signature modlid) @ sign
  end (signature "Pervasives") paths_opened in
  possibilities


  with ex -> (printf "%S\n%!" (Printexc.to_string ex); raise ex)) in

  let possibilities = possibilities @ modules in
  let pat = Str.regexp_case_fold (Str.quote (on_search())) in
  let possibilities = List.filter begin fun (name, _, _, _, _) ->
    Str.string_partial_match pat name 0
  end possibilities in
  let possibilities = List.sort begin fun (n1, _, _, _, _) (n2, _, _, _, _) ->
    String.compare (String.lowercase n1) (String.lowercase n2)
    end possibilities in
  if possibilities = [] then (raise Not_found);
  let p = new popup ~on_row_activated ~on_type ~on_search
    ~show_module:true ~possibilities () in
  p#set_title ("");
  p


(* Autocompletamento per ident *)

(*let ident_lookup ~paths_opened ~on_row_activated ~on_type ~on_search =
  let possibilities = List.fold_left begin fun sign modlid ->
    (signature modlid) @ sign
  end (signature "Pervasives") paths_opened in
  let pat = Str.regexp_case_fold ((Str.quote (on_search()))^".*") in
  let possibilities = List.filter begin fun (name, _, _, _) ->
    Str.string_match pat name 0
  end possibilities in
  let possibilities = List.sort begin fun (n1, _, _, _) (n2, _, _, _) ->
    String.compare (String.lowercase n1) (String.lowercase n2)
    end possibilities in
  let p = new popup ~on_row_activated ~on_type ~on_search
    ~show_module:true ~possibilities () in
  p
*)

(* Autocompletamento per punto (Modulo) *)

let dot_lookup ~modlid ~on_row_activated ~on_type ~on_search =
  let possibilities = signature modlid in
  let p = new popup ~modlid ~on_row_activated ~on_type ~on_search ~possibilities () in
  p#set_title ("Module <tt>"^modlid^"</tt>");
  p





