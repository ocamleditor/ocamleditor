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
open Pref_page

(** pref_color *)
class pref_color title ?packing () =
  let vbox                = GPack.vbox ~spacing ?packing () in
  let notebook            = GPack.notebook ~packing:vbox#add () in
  let border_width        = 5 in
  (* tags *)
  let cols                = new GTree.column_list in
  let tag_col             = cols#add Gobject.Data.string in
  let lab_col             = cols#add Gobject.Data.string in
  let tag_model           = GTree.list_store cols in
  (* Syntax coloring *)
  let color_ocaml         = GPack.vbox ~border_width ~spacing:8 () in
  let _                   = notebook#append_page ~tab_label:(GMisc.label ~text:"OCaml" ())#coerce color_ocaml#coerce in
  let hbox                = GPack.hbox ~spacing:8 ~packing:color_ocaml#pack () in
  let _                   = GMisc.label ~text:"Default background color:" ~xalign ~packing:hbox#pack () in
  let button_default_bg   = GButton.color_button ~packing:hbox#pack () in
  let _                   = button_default_bg#set_relief `NONE in
  let box_tag             = GPack.hbox ~border_width:0 ~spacing:8 ~packing:color_ocaml#pack () in
  let sw                  = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:box_tag#pack () in
  let view_tag            = GTree.view ~width:200 ~height:150 ~headers_visible:false ~model:tag_model ~packing:sw#add () in
  let renderer            = GTree.cell_renderer_text [] in
  let vc_tag              = GTree.view_column ~renderer:(renderer, ["text", tag_col]) () in
  let _                   = view_tag#append_column vc_tag in
  let vc_lab              = GTree.view_column ~title:"Text Elements" ~renderer:(renderer, ["text", lab_col]) () in
  let _                   = view_tag#append_column vc_lab in
  let _                   = vc_tag#set_visible false in
  let prop_box            = GPack.vbox ~border_width:0 ~packing:box_tag#add () in
  let table               = GPack.table ~packing:prop_box#pack () in
  let label_tag_bg        = GMisc.label ~text:"Background:" ~xalign:0.0 ~packing:(table#attach ~top:0 ~left:0) () in
  let button_tag_bg       = GButton.color_button ~packing:(table#attach ~top:0 ~left:1) () in
  let _                   = button_tag_bg#set_relief `NONE in
  let check_tag_bg        = GButton.check_button ~label:"Use default" ~packing:(table#attach ~top:0 ~left:2) () in
  let _                   = GMisc.label ~text:"Foreground:" ~xalign:0.0 ~packing:(table#attach ~top:1 ~left:0) () in
  let button_tag_fg       = GButton.color_button ~packing:(table#attach ~top:1 ~left:1) () in
  let _                   = button_tag_fg#set_relief `NONE in
  let check_tag_weight    = GButton.check_button ~label:"Bold" ~packing:(table#attach ~top:2 ~left:0) () in
  let check_tag_style     = GButton.check_button ~label:"Italic" ~packing:(table#attach ~top:3 ~left:0) () in
  let check_tag_underline = GButton.check_button ~label:"Underline" ~packing:(table#attach ~top:4 ~left:0) () in
  let _                   = check_tag_weight#set_image (GMisc.image ~stock:`BOLD ())#coerce in
  let _                   = check_tag_style#set_image (GMisc.image ~stock:`ITALIC ())#coerce in
  let _                   = check_tag_underline#set_image (GMisc.image ~stock:`UNDERLINE ())#coerce in
  let box_odoc_bg         = GPack.hbox ~spacing:5 ~packing:prop_box#pack ~show:false () in
  let _                   = GMisc.label ~xalign:0.0
    ~markup:"Paragraph background colors:" (* \n<small>(only applies to comments preceded\nby a blank line)</small> *)
    ~packing:box_odoc_bg#pack () in
  let button_odoc_bg      = GButton.color_button ~packing:box_odoc_bg#pack () in
  let _                   = button_odoc_bg#set_relief `NONE in
  let button_odoc_bg2     = GButton.color_button ~packing:box_odoc_bg#pack () in
  let _                   = button_odoc_bg2#set_relief `NONE in
  (* OCaml Preview *)
  let osw                 = GBin.scrolled_window ~height:200 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~shadow_type:`IN () in
  let buffer              = new Ocaml_text.buffer ~lexical_enabled:true () in
  let _                   = buffer#set_text ocaml_preview in
  let preview             = new Ocaml_text.view ~buffer () in
  let _                   = Gmisclib.Idle.add (fun () -> preview#buffer#place_cursor ~where:(preview#buffer#start_iter#forward_lines 5)) in
  let _                   = Preferences_apply.apply (preview :> Text.view) Preferences.preferences#get in
  let _                   = preview#set_editable false in
  let _                   = osw#add preview#coerce in
  let _                   = color_ocaml#add osw#coerce in
  (* color_compl *)
  let color_compl         = GPack.vbox ~border_width () in
  let table               = GPack.table ~row_spacings ~col_spacings ~packing:color_compl#pack () in
  let _                   = GMisc.label ~text:"Background color:" ~xalign ~packing:(table#attach ~top:0 ~left:0) () in
  let _                   = GMisc.label ~text:"Foreground color:" ~xalign ~packing:(table#attach ~top:1 ~left:0) () in
  let button_tag_bg_popup = GButton.color_button ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let button_tag_fg_popup = GButton.color_button ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
  let _                   = notebook#append_page ~tab_label:(GMisc.label ~text:"Completion" ())#coerce color_compl#coerce in
  (* color_other *)
  let color_other         = GPack.vbox ~border_width () in
  let table               = GPack.table ~row_spacings ~col_spacings ~packing:color_other#pack () in
  let button_bg           = GButton.color_button ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let button_fg_stdin     = GButton.color_button ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
  let button_fg_stdout    = GButton.color_button ~packing:(table#attach ~top:2 ~left:1 ~expand:`X) () in
  let button_fg_err       = GButton.color_button ~packing:(table#attach ~top:3 ~left:1 ~expand:`X) () in
  let button_fg_warn      = GButton.color_button ~packing:(table#attach ~top:4 ~left:1 ~expand:`X) () in
  let _                   = GMisc.label ~xalign ~text:"Background color:" ~packing:(table#attach ~top:0 ~left:0) () in
  let _                   = GMisc.label ~xalign ~text:"Standard input:" ~packing:(table#attach ~top:1 ~left:0) () in
  let _                   = GMisc.label ~xalign ~text:"Standard output:" ~packing:(table#attach ~top:2 ~left:0) () in
  let _                   = GMisc.label ~xalign ~text:"Errors:" ~packing:(table#attach ~top:3 ~left:0) () in
  let _                   = GMisc.label ~xalign ~text:"Warnings:" ~packing:(table#attach ~top:4 ~left:0) () in
  (* *)
  let _                   = notebook#append_page ~tab_label:(GMisc.label ~text:"Message Pane" ())#coerce color_other#coerce in
object (self)
  inherit page title vbox
  val mutable tags = []
  val mutable current_tag = ""
  val mutable signals = []

  initializer
    ignore (view_tag#selection#connect#after#changed ~callback:self#read_tags);
    signals <- [
      check_tag_bg#coerce,        check_tag_bg#connect#toggled ~callback:self#update_preview;
      button_tag_fg#coerce,       button_tag_fg#connect#color_set ~callback:self#update_preview;
      button_tag_bg#coerce,       button_tag_bg#connect#color_set ~callback:self#update_preview;
      button_default_bg#coerce,   button_default_bg#connect#color_set ~callback:self#update_preview;
      button_odoc_bg#coerce,      button_odoc_bg#connect#color_set ~callback:self#update_preview;
      button_odoc_bg2#coerce,     button_odoc_bg2#connect#color_set ~callback:self#update_preview;
      check_tag_weight#coerce,    check_tag_weight#connect#clicked ~callback:self#update_preview;
      check_tag_style#coerce,     check_tag_style#connect#clicked ~callback:self#update_preview;
      check_tag_underline#coerce, check_tag_underline#connect#clicked ~callback:self#update_preview;
    ];
    ignore (check_tag_bg#connect#after#clicked ~callback:(fun () ->
      button_tag_bg#misc#set_sensitive (not check_tag_bg#active)));


  method write pref =
    pref.Preferences.pref_bg_color_popup <- color_name button_tag_bg_popup#color;
    pref.Preferences.pref_fg_color_popup <- color_name button_tag_fg_popup#color;
    pref.Preferences.pref_bg_color <- (color_name button_default_bg#color, false);
    pref.Preferences.pref_tags <- tags;
    let ltags, prop = List.split tags in
    Lexical.tags := ltags;
    Lexical.colors := prop;
    pref.Preferences.pref_output_bg <- color_name button_bg#color;
    pref.Preferences.pref_output_fg_stdin <- color_name button_fg_stdin#color;
    pref.Preferences.pref_output_fg_stdout <- color_name button_fg_stdout#color;
    pref.Preferences.pref_output_fg_err <- color_name button_fg_err#color;
    pref.Preferences.pref_output_fg_warn <- color_name button_fg_warn#color;
    pref.Preferences.pref_ocamldoc_paragraph_bgcolor_1 <- Some (color_name button_odoc_bg#color);
    pref.Preferences.pref_ocamldoc_paragraph_bgcolor_2 <- Some (color_name button_odoc_bg2#color);

  method read pref =
    button_tag_bg_popup#set_color (GDraw.color (`NAME pref.Preferences.pref_bg_color_popup));
    button_tag_fg_popup#set_color (GDraw.color (`NAME pref.Preferences.pref_fg_color_popup));
    tags <- List.sort Pervasives.compare pref.Preferences.pref_tags;
    button_default_bg#set_color (GDraw.color (`NAME (fst pref.Preferences.pref_bg_color)));
    tag_model#clear();
    List.iter begin fun tag ->
      try
        let row = tag_model#append () in
        let label = List.assoc (fst tag) Preferences.tag_labels in
        tag_model#set ~row ~column:tag_col (fst tag);
        tag_model#set ~row ~column:lab_col label;
      with Not_found -> begin
        fprintf stderr "Preferences_tool, tag not found: %S\n%!" (fst tag);
      end
    end tags;
    view_tag#selection#select_path (GTree.Path.create [0]);
    button_bg#set_color (GDraw.color (`NAME pref.Preferences.pref_output_bg));
    button_fg_stdin#set_color (GDraw.color (`NAME pref.Preferences.pref_output_fg_stdin));
    button_fg_stdout#set_color (GDraw.color (`NAME pref.Preferences.pref_output_fg_stdout));
    button_fg_err#set_color (GDraw.color (`NAME pref.Preferences.pref_output_fg_err));
    button_fg_warn#set_color (GDraw.color (`NAME pref.Preferences.pref_output_fg_warn));
    Gaux.may pref.Preferences.pref_ocamldoc_paragraph_bgcolor_1 ~f:(fun color -> button_odoc_bg#set_color (GDraw.color (`NAME color)));
    Gaux.may pref.Preferences.pref_ocamldoc_paragraph_bgcolor_2 ~f:(fun color -> button_odoc_bg2#set_color (GDraw.color (`NAME color)));
    self#update_preview ();

  method private read_tags () =
    match view_tag#selection#get_selected_rows with
      | path :: _ ->
        List.iter (fun (w, s) -> w#misc#handler_block s) signals;
        let row = tag_model#get_iter path in
        let tname = tag_model#get ~row ~column:tag_col in
        current_tag <- tname;
        begin
          match List_opt.assoc tname tags with
            | Some (color, weight, style, underline, _, (bg_default, bg_color)) ->
              button_tag_fg#set_color (GDraw.color color);
              check_tag_weight#set_active (weight <> `NORMAL);
              check_tag_style#set_active (style <> `NORMAL);
              check_tag_underline#set_active (underline <> `NONE);
              check_tag_bg#set_active bg_default;
              button_tag_bg#set_color (GDraw.color bg_color);
            | _ -> ()
        end;
        if Oe_config.ocamldoc_paragraph_bgcolor_enabled && tname = "ocamldoc"
        then begin
          box_odoc_bg#misc#show();
          label_tag_bg#misc#set_sensitive false;
          button_tag_bg#misc#set_sensitive false;
          check_tag_bg#misc#set_sensitive false;
        end else begin
          box_odoc_bg#misc#hide();
          label_tag_bg#misc#set_sensitive true;
          button_tag_bg#misc#set_sensitive true;
          check_tag_bg#misc#set_sensitive true;
        end;
        button_tag_bg#misc#set_sensitive (not check_tag_bg#active);
        self#update_preview();
        List.iter (fun (w, s) -> w#misc#handler_unblock s) signals;
      | [] -> ()

  method private update_preview () =
    let color      = button_tag_fg#color in
    let weight     = if check_tag_weight#active then `BOLD else `NORMAL in
    let style      = if check_tag_style#active then `ITALIC else `NORMAL in
    let underline  = if check_tag_underline#active then `SINGLE else `NONE in
    let bg_default = check_tag_bg#active in
    let bg_color   = button_tag_bg#color in
    if current_tag <> "" then begin
      tags <- (current_tag,
        (`NAME (color_name color), weight, style, underline, `MEDIUM, (bg_default, `NAME (color_name bg_color)))) ::
          (List.remove_assoc current_tag tags);
    end;
    let temp_pref = {Preferences.preferences#get with
      Preferences.pref_bg_color = (color_name button_default_bg#color, false);
      Preferences.pref_tags = tags;
      Preferences.pref_ocamldoc_paragraph_bgcolor_1 = Some (color_name button_odoc_bg#color);
      Preferences.pref_ocamldoc_paragraph_bgcolor_2 = Some (color_name button_odoc_bg2#color);
    } in
    let tag_names, colors = List.split tags in
    preview#tbuffer#init_tags ~tags:tag_names ~colors
      ~ocamldoc_paragraph_bgcolor_1:temp_pref.Preferences.pref_ocamldoc_paragraph_bgcolor_1
      ~ocamldoc_paragraph_bgcolor_2:temp_pref.Preferences.pref_ocamldoc_paragraph_bgcolor_2 ();
    Lexical.tag (preview#buffer :> GText.buffer);
    Preferences_apply.apply (preview :> Text.view) temp_pref;
end

(** pref_color_structure *)
and pref_color_structure title ?packing () =
  let vbox                = GPack.vbox ~spacing ?packing () in
  let table               = GPack.table ~row_spacings ~col_spacings ~packing:vbox#pack () in
  let _                   = GMisc.label ~text:"Base background color:" ~xalign ~packing:(table#attach ~top:0 ~left:0) () in
  let _                   = GMisc.label ~text:"Base foreground color:" ~xalign ~packing:(table#attach ~top:1 ~left:0) () in
  let _                   = GMisc.label ~text:"Selected item background color:" ~xalign ~packing:(table#attach ~top:2 ~left:0) () in
  let _                   = GMisc.label ~text:"Selected item foreground color:" ~xalign ~packing:(table#attach ~top:3 ~left:0) () in
  let _                   = GMisc.label ~text:"Active item background color:" ~xalign ~packing:(table#attach ~top:4 ~left:0) () in
  let _                   = GMisc.label ~text:"Active item foreground color:" ~xalign ~packing:(table#attach ~top:5 ~left:0) () in
  let _                   = GMisc.label ~text:"Types foreground color:" ~xalign ~packing:(table#attach ~top:6 ~left:0) () in
  let check_alt_rows      = GButton.check_button ~label:"Draw rows in alternating colors" ~packing:(table#attach ~top:7 ~left:0) () in
  let button_color_nor_bg = GButton.color_button ~packing:(table#attach ~top:0 ~left:1 ~expand:`X) () in
  let button_color_nor_fg = GButton.color_button ~packing:(table#attach ~top:1 ~left:1 ~expand:`X) () in
  let button_color_sel_bg = GButton.color_button ~packing:(table#attach ~top:2 ~left:1 ~expand:`X) () in
  let button_color_sel_fg = GButton.color_button ~packing:(table#attach ~top:3 ~left:1 ~expand:`X) () in
  let button_color_act_bg = GButton.color_button ~packing:(table#attach ~top:4 ~left:1 ~expand:`X) () in
  let button_color_act_fg = GButton.color_button ~packing:(table#attach ~top:5 ~left:1 ~expand:`X) () in
  let button_color_types  = GButton.color_button ~packing:(table#attach ~top:6 ~left:1 ~expand:`X) () in
object
  inherit page title vbox
  method read pref =
    button_color_types#set_color (GDraw.color (`NAME pref.Preferences.pref_outline_color_types));
    button_color_nor_bg#set_color (GDraw.color (`NAME pref.Preferences.pref_outline_color_nor_bg));
    button_color_nor_fg#set_color (GDraw.color (`NAME pref.Preferences.pref_outline_color_nor_fg));
    button_color_sel_bg#set_color (GDraw.color (`NAME pref.Preferences.pref_outline_color_sel_bg));
    button_color_sel_fg#set_color (GDraw.color (`NAME pref.Preferences.pref_outline_color_sel_fg));
    button_color_act_bg#set_color (GDraw.color (`NAME pref.Preferences.pref_outline_color_act_bg));
    button_color_act_fg#set_color (GDraw.color (`NAME pref.Preferences.pref_outline_color_act_fg));
    check_alt_rows#set_active (pref.Preferences.pref_outline_color_alt_rows <> None);

  method write pref =
    pref.Preferences.pref_outline_color_types <- color_name button_color_types#color;
    pref.Preferences.pref_outline_color_nor_bg <- color_name button_color_nor_bg#color;
    pref.Preferences.pref_outline_color_nor_fg <- color_name button_color_nor_fg#color;
    pref.Preferences.pref_outline_color_sel_bg <- color_name button_color_sel_bg#color;
    pref.Preferences.pref_outline_color_sel_fg <- color_name button_color_sel_fg#color;
    pref.Preferences.pref_outline_color_act_bg <- color_name button_color_act_bg#color;
    pref.Preferences.pref_outline_color_act_fg <- color_name button_color_act_fg#color;
    pref.Preferences.pref_outline_color_alt_rows <- if check_alt_rows#active then
        Some 0.95
      else None

end
