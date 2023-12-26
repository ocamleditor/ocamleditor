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
open Miscellanea
open Odoc_info

module Database =
struct
  (** ocamldoc_command *)
  let ocamldoc_command ~project ~filename () =
    try
      let file =
        let mli = sprintf "%s.mli" (Filename.chop_extension filename) in
        if Sys.file_exists mli then Some mli else
          let ml = sprintf "%s.ml" (Filename.chop_extension filename) in
          if Sys.file_exists ml then Some ml else
          None
      in
      begin
        match file with
          | Some file ->
            let out_filename = Filename.temp_file (Filename.basename (Filename.chop_extension filename)) ".tmp" in
            let search_path = Project.get_search_path_i_format project in
            let args =
              Array.concat [
                [|
                  "-dump";
                  ((*Shell.quote_arg *)out_filename);
                  "-I"; "+threads";
                |];
                (Array.of_list (Miscellanea.split " +" search_path));
                [|
                  (*Shell.quote_arg*) file;
                |]
              ]
            in
            ignore (Spawn.sync (*~process_in:Spawn.redirect_to_stdout ~process_err:Spawn.redirect_to_stderr*) (Ocaml_config.ocamldoc()) args);
            Some out_filename
          | _ -> None
      end;
    with Invalid_argument _ -> None;;

  (*(** read *)
  let read (project, filename) =
    let odoc = ocamldoc ~project ~filename () in
    let time = Unix.gettimeofday () in
    match odoc with
      | Some filename ->
        let module_list = Odoc_info.load_modules filename in
        let elements = Odoc_info.Search.search_by_name module_list (Miscellanea.regexp "") in
        let i = ref (-1) in
        let elements = List.map begin fun res ->
          incr i;
          match res with
            | Search.Res_type elem -> (!i, elem.Type.ty_name, Some Oe.Ptype), res
            | Search.Res_value elem -> (!i, elem.Value.val_name, Some Oe.Pvalue), res
            | Search.Res_module elem -> (!i, elem.Module.m_name, Some Oe.Pmodule), res
            | Search.Res_module_type elem -> (!i, elem.Module.mt_name, Some Oe.Pmodtype), res
            | Search.Res_class elem -> (!i, elem.Class.cl_name, Some Oe.Pclass), res
            | Search.Res_class_type elem -> (!i, elem.Class.clt_name, Some Oe.Pcltype), res
            | Search.Res_exception elem -> (!i, elem.Exception.ex_name, Some Oe.Pexception), res
            | Search.Res_attribute elem -> (!i, elem.Value.att_value.Value.val_name, Some Oe.Pattribute), res
            | Search.Res_method elem -> (!i, elem.Value.met_value.Value.val_name, Some Oe.Pmethod), res
            | Search.Res_section (name, text) -> (!i, name, None), res
        end elements in
        if Sys.file_exists filename then (Sys.remove filename);
        elements, time
      | _ -> [], time;;*)

  let read' (project, filename) =
    let odoc = ocamldoc_command ~project ~filename () in
    let time = Unix.gettimeofday () in
    match odoc with
      | Some filename ->
        let module_list = Odoc_info.load_modules filename in
        if Sys.file_exists filename then (Sys.remove filename);
        module_list, time
      | _ -> [], time;;

  (** read_memo *)
  let read_memo = Miscellanea.Memo.fast ~f:read';;

  (** read *)
  let read ~project ~symbol =
    let force (_, ts) =
      let filename = symbol.Oe.sy_filename in
      ts < (Unix.stat filename).Unix.st_mtime
    in
    let module_list, _ = read_memo ~force (project, symbol.Oe.sy_filename) in
    module_list

  (** find_name *)
  let find_name ~project ~symbol =
    let value_path = Symbols.concat_value_path symbol in
    let module_list = read ~project ~symbol in
    let re = kprintf Str.regexp "%s$" (Str.quote value_path) in
    Odoc_info.Search.search_by_name module_list re;;

  (** find_module *)
  let find_module ~project ~symbol =
    let module_list = read ~project ~symbol in
    let module_name = Symbols.get_module_name symbol in
    List_opt.find (fun m -> m.Odoc_info.Module.m_name = module_name) module_list

end

module Printer =
struct

  let pending_newline = ref false

  let insert_newline ~(buffer : GText.buffer) ftag =
    let (!!) = ftag in
    (*let iter = buffer#get_iter `INSERT in
    if iter#line_index > 0 then*) (buffer#insert ~tags:[!!`LINE_SPACING_SMALL] "\n");;

  let concat_raw_text text =
    let repl = if !pending_newline then (pending_newline := false; "") else "\n" in
    let text = Str.global_replace (Miscellanea.regexp "^\n[ *]+") repl text in
    let text = Str.global_replace (Miscellanea.regexp "\n[ *]+$") " " text in
    let text = Str.global_replace (Miscellanea.regexp "\n[ *]+") " " text in
    text;;

  let rec insert_text ?concat buffer text ftag =
    List.iter (insert_elem ?concat buffer ftag) text

  and insert_elem ?(concat=concat_raw_text) (buffer : GText.buffer) ftag =
    let (!!) = ftag in
    function
      | Raw text ->
        let text = concat text in
        if text.[0] = '\n' then begin
          buffer#insert ~tags:[!!`LINE_SPACING_SMALL] "\n";
          buffer#insert (Str.string_after text 1)
        end else buffer#insert text;
      | Code text ->
        buffer#insert ~tags:[!!`TT] text
      | CodePre text ->
        insert_newline ~buffer ftag;
        buffer#insert ~tags:[!!`SMALL] "\n";
        buffer#insert ~tags:[!!`TT;!!`LEFT_MARGIN] text;
        buffer#insert ~tags:[!!`SMALL] "\n";
      | Verbatim text ->
        buffer#insert ~tags:[!!`TT] text;
      | Bold text ->
        Gtk_util.with_tag !!`BOLD ~buffer (fun () -> insert_text buffer text ftag)
      | Italic text ->
        Gtk_util.with_tag !!`ITALIC ~buffer (fun () -> insert_text buffer text ftag)
      | Emphasize text ->
        Gtk_util.with_tag !!`ITALIC ~buffer (fun () -> insert_text buffer text ftag)
      | Center text ->
        Gtk_util.with_tag !!`CENTER ~buffer (fun () -> insert_text buffer text ftag)
      | Left text ->
        Gtk_util.with_tag !!`LEFT ~buffer (fun () -> insert_text buffer text ftag)
      | Right text ->
        Gtk_util.with_tag !!`RIGHT ~buffer (fun () -> insert_text buffer text ftag)
      | List elems ->
        let iter = buffer#get_iter `INSERT in
        let c = iter#get_text ~stop:iter#backward_char in
        if c <> "\n" then insert_newline ~buffer ftag;
        buffer#insert ~tags:[!!`SMALL] "\n";
        let i = ref 0 in
        List.iter begin fun text ->
          Gtk_util.with_tag !!`LI ~buffer begin fun () ->
            buffer#insert "• ";
            insert_text buffer text ftag;
            (*if !i <> last then*) insert_newline ~buffer ftag;
          end;
          incr i;
        end elems;
        buffer#insert ~tags:[!!`SMALL] "\n";
        pending_newline := true;
      | Enum elems ->
        let iter = buffer#get_iter `INSERT in
        let c = iter#get_text ~stop:iter#backward_char in
        if c <> "\n" then insert_newline ~buffer ftag;
        buffer#insert ~tags:[!!`SMALL] "\n";
        let i = ref 0 in
        List.iter begin fun text ->
          Gtk_util.with_tag !!`LI ~buffer begin fun () ->
            buffer#insert (sprintf "%d. " (!i + 1));
            insert_text buffer text ftag;
            insert_newline ~buffer ftag;
          end;
          incr i;
        end elems;
        buffer#insert ~tags:[!!`SMALL] "\n";
        pending_newline := true;
      | Newline ->
        insert_newline ~buffer ftag
      | Block text ->
        insert_newline ~buffer ftag;
        insert_text buffer text ftag;
        insert_newline ~buffer ftag;
      | Title (n, _(*opt_label*), text) ->
        if not !pending_newline then (insert_newline ~buffer ftag; pending_newline := false);
        Gtk_util.with_tag !!(`TITLE n) ~buffer (fun () -> insert_text buffer text ftag);
        insert_newline ~buffer ftag;
      | Latex text ->
        buffer#insert text;
      | Link (_(*refer*), text) ->
        Gtk_util.with_tag !!`TT ~buffer begin fun () ->
          insert_text ~concat buffer text ftag;
        end
      (*| Target _ -> ()
      | Ref (text, _, _) ->*)
      | Target _ -> ()
      | Ref (text, _, _) ->
        buffer#insert ~tags:[!!`SANS] text;
      | Superscript text ->
        Gtk_util.with_tag !!`SUPERSCRIPT ~buffer (fun () -> insert_text buffer text ftag)
      | Subscript text ->
        Gtk_util.with_tag !!`SUBSCRIPT ~buffer (fun () -> insert_text buffer text ftag)
      | Module_list mlist ->
        List.iter begin fun mo ->
          buffer#insert mo;
          insert_newline ~buffer ftag;
        end mlist
      | Index_list ->
          buffer#insert "Index List";
      | Custom (_(*string*), text) ->
        insert_text buffer text ftag;
  ;;

  module Info = struct
    let insert_params (!!) info (buffer : GText.buffer) =
      if info.i_params <> [] then begin
        insert_newline ~buffer (!!);
        buffer#insert ~tags:[!!`PARAGRAPH] "Parameters ";
        insert_newline ~buffer (!!);
        List.iter begin fun (param, text) ->
          buffer#insert ~tags:[!!`PARAM] param;
          if text <> [] then
            Gtk_util.with_tag !!`PARAM_DESCR ~buffer begin fun () ->
              buffer#insert " - ";
              insert_text buffer text (!!);
              insert_newline ~buffer (!!);
            end;
        end info.i_params;
        let stop = buffer#get_iter `INSERT in
        buffer#delete ~start:stop#backward_char ~stop;
      end;;

    let insert_return_value (!!) info (buffer : GText.buffer) =
      match info.i_return_value with
        | Some text_elements ->
          insert_newline ~buffer (!!);
          buffer#insert ~tags:[!!`PARAGRAPH] "Returns ";
          insert_newline ~buffer (!!);
          Gtk_util.with_tag !!`LEFT_MARGIN ~buffer (fun () -> insert_text buffer text_elements (!!));
        | _ -> ();;

    let insert_raised_exceptions (!!) info (buffer : GText.buffer) =
      match info.i_raised_exceptions with
        | [] -> ()
        | _ ->
          insert_newline ~buffer (!!);
          buffer#insert ~tags:[!!`PARAGRAPH] "Raises ";
          List.iter begin fun (name, text) ->
            insert_newline ~buffer (!!);
            buffer#insert ~tags:[!!`LEFT_MARGIN; !!`TTB] name;
            Gtk_util.with_tag !!`LEFT_MARGIN ~buffer begin fun () ->
              buffer#insert " ";
              insert_text buffer text (!!);
            end;
          end info.i_raised_exceptions;;

    let insert_deprecated (!!) info (buffer : GText.buffer) =
      match info.i_deprecated with
        | Some text_elements ->
          insert_newline ~buffer (!!);
          buffer#insert ~tags:[!!`PARAGRAPH] "Deprecated ";
          insert_newline ~buffer (!!);
          Gtk_util.with_tag !!`LEFT_MARGIN ~buffer (fun () -> insert_text buffer text_elements (!!));
        | _ -> ();;

    let insert_version (!!) info (buffer : GText.buffer) =
      match info.i_version with
        | Some text ->
          insert_newline ~buffer (!!);
          buffer#insert ~tags:[!!`PARAGRAPH] "Version ";
          insert_newline ~buffer (!!);
          Gtk_util.with_tag !!`LEFT_MARGIN ~buffer (fun () -> buffer#insert text);
        | _ -> ();;

    let insert_since (!!) info (buffer : GText.buffer) =
      match info.i_since with
        | Some text ->
          insert_newline ~buffer (!!);
          buffer#insert ~tags:[!!`PARAGRAPH] "Since ";
          insert_newline ~buffer (!!);
          Gtk_util.with_tag !!`LEFT_MARGIN ~buffer (fun () -> buffer#insert text);
        | _ -> ();;

    let insert_authors (!!) info (buffer : GText.buffer) =
      match info.i_authors with
        | [] -> ()
        | _ ->
          insert_newline ~buffer (!!);
          buffer#insert ~tags:[!!`PARAGRAPH] "Author(s) ";
          insert_newline ~buffer (!!);
          let authors = String.concat ", " info.i_authors in
          Gtk_util.with_tag !!`LEFT_MARGIN ~buffer (fun () -> buffer#insert authors);;

    let insert_sees (!!) info (buffer : GText.buffer) =
      match info.i_sees with
        | [] -> ()
        | _ ->
          insert_newline ~buffer (!!);
          buffer#insert ~tags:[!!`PARAGRAPH] "See Also ";
          insert_newline ~buffer (!!);
          List.iter begin fun (see_ref, text) ->
            insert_newline ~buffer (!!);
            begin
              match see_ref with
                | Odoc_info.See_url s ->
                  buffer#insert ~tags:[!!`LEFT_MARGIN; !!`SANS] s;
                | Odoc_info.See_file s ->
                  buffer#insert ~tags:[!!`LEFT_MARGIN; !!`TT] s;
                | Odoc_info.See_doc s ->
                  buffer#insert ~tags:[!!`LEFT_MARGIN; !!`ITALIC] s;
            end;
            if text <> [] then
              Gtk_util.with_tag !!`PARAM_DESCR ~buffer begin fun () ->
                buffer#insert " - ";
                insert_text buffer text (!!);
              end;
          end info.i_sees;;

    let insert_custom (!!) info (buffer : GText.buffer) =
      match info.i_custom with
        | [] -> ()
        | _ ->
          insert_newline ~buffer (!!);
          buffer#insert ~tags:[!!`PARAGRAPH] " ";
          insert_newline ~buffer (!!);
          List.iter begin fun (tag, text) ->
            insert_newline ~buffer (!!);
            buffer#insert ~tags:[!!`BOLD] tag;
            if text <> [] then begin
              buffer#insert " - ";
              insert_text buffer text (!!)
            end;
          end info.i_custom;;

    let insert_desc ~newline_before (!!) info (buffer : GText.buffer) =
      match info.i_desc with
        | Some text_elements ->
          if newline_before then (buffer#insert ~tags:[!!`LINE_SPACING_SMALL] "\n\n");
          insert_text buffer text_elements (!!);
        | _ -> if newline_before then (buffer#insert "\n\n");;

    let insert_info ?(newline_before=true) (!!) (buffer : GText.buffer) =
      function Some info ->
        insert_desc ~newline_before (!!) info buffer;
        (*insert_newline ~buffer;*)
        insert_authors (!!) info buffer;
        insert_version (!!) info buffer;
        insert_sees (!!) info buffer;
        insert_since (!!) info buffer;
        insert_deprecated (!!) info buffer;
        insert_params (!!) info buffer;
        insert_raised_exceptions (!!) info buffer;
        insert_return_value (!!) info buffer;
        insert_custom (!!) info buffer;
      | _ -> ();;
  end

  (** insert_type *)
  let insert_type ~(buffer : GText.buffer) (!!) elem =
    let insert_type = buffer#insert ~tags:[!!`TYPE] in
    let m1 = buffer#create_mark (buffer#get_iter `INSERT) in
    begin
      (*let info = match elem.Type.ty_info with Some x -> Odoc_info.string_of_info x | _ -> "" in
      let re = kprintf Str.regexp "[\r\n]*%s[\r\n]*" (Str.quote info) in
      let typ = Odoc_info.string_of_type elem in
      let typ = Str.global_replace re "" typ in
      buffer#insert typ;*)
      let insert_type_element_comment text =
        Gtk_util.with_tag !!`TYPE_COMMENT ~buffer begin fun () ->
          buffer#insert " (\x2A ";
          insert_text buffer text (!!);
          buffer#insert " \x2A)\n";
        end
      in
      buffer#insert ~tags:[!!`TYPE] "type ";
      if elem.Type.ty_parameters <> [] then begin
        Odoc_info.reset_type_names();
        insert_type "(";
        let params = List.map (fun (te, _, _) -> Odoc_info.string_of_type_expr te) elem.Type.ty_parameters in
        insert_type (String.concat ", " params);
        insert_type ") ";
      end;
      insert_type (Odoc_info.Name.simple elem.Type.ty_name);
      match elem.Type.ty_kind with
        | Type.Type_abstract ->
          begin
            match [@warning "-4"] elem.Type.ty_manifest with
              | Some (Odoc_type.Other te) ->
                Odoc_info.reset_type_names();
                insert_type " = ";
                insert_type (Odoc_info.string_of_type_expr te)
              | _ -> ()
          end;
        | Type.Type_variant vcl ->
          let (!!!) = function
            | { Odoc_type.vc_args = Odoc_type.Cstr_tuple core_types; _ } ->
              `Tuple, List.map begin fun core_type ->
                Odoc_info.reset_type_names ();
                let te = Odoc_info.string_of_type_expr core_type in
                Str.global_replace (Str.regexp_string ((Name.father elem.Type.ty_name) ^ ".")) "" te
              end core_types
            | { Odoc_type.vc_args = Odoc_type.Cstr_record fields; _ } ->
             	Odoc_info.reset_type_names ();
              `Record, [Odoc_info.string_of_record fields] (* ??? More *)
          in
          insert_type " = \n";
          let maxlength = List.fold_left begin fun acc vc ->
            let _, args = !!! vc in
            let args = String.concat " * " args in
            let len = String.length vc.Type.vc_name + String.length args + 4 in
            max acc len
          end 0 vcl in
          List.iter begin fun vc ->
            buffer#insert ~tags:[!!`TYPE; !!`INDENT] "  | ";
            let code = vc.Type.vc_name ^ begin
                let kind, args = !!! vc in
                if kind = `Tuple then
                  if args = [] then "" else " of " ^ String.concat " * " args
                else
                  " of { " ^ String.concat " * " args ^ " }"
            end in
            insert_type (Miscellanea.rpad code ' ' maxlength);
            match vc.Type.vc_text with
              | Some { i_desc = Some text; _ } -> insert_type_element_comment text
              | _ -> insert_newline ~buffer (!!)
          end vcl
        | Type.Type_record rfl ->
          insert_type " = {\n";
          let maxlength = List.fold_left begin fun acc rf ->
            let len = (String.length rf.Type.rf_name) + (if rf.Type.rf_mutable then 8 else 0) in
            max acc len
          end 0 rfl in
          let (!!!) rf =
            Odoc_info.reset_type_names();
            let te = Odoc_info.string_of_type_expr rf.Type.rf_type in
            Str.global_replace (Str.regexp_string ((Name.father elem.Type.ty_name) ^ ".")) "" te
          in
          let maxlength_te = List.fold_left begin fun acc rf ->
            let len = String.length (!!! rf) in
            max acc len
          end 0 rfl in
          List.iter begin fun rf ->
            if rf.Type.rf_mutable then begin
              let s = Miscellanea.rpad ("mutable " ^ rf.Type.rf_name) ' ' maxlength in
              buffer#insert ~tags:[!!`TYPE; !!`INDENT] "  ";
              buffer#insert ~tags:[!!`TYPE; !!`INDENT] s
            end else begin
              let s = Miscellanea.rpad rf.Type.rf_name ' ' maxlength in
              buffer#insert ~tags:[!!`TYPE; !!`INDENT] "  ";
              buffer#insert ~tags:[!!`TYPE; !!`INDENT] s
            end;
            buffer#insert ~tags:[!!`TYPE; !!`INDENT] " : ";
            buffer#insert ~tags:[!!`TYPE; !!`INDENT]
              (Miscellanea.rpad (!!! rf) ' ' maxlength_te);
            match rf.Type.rf_text with
              | Some { i_desc = Some text; _ } -> insert_type_element_comment text
              | _ -> insert_newline ~buffer (!!)
          end rfl;
          insert_type "}";
        (* since 4.02.0  TODO *)
        | Type.Type_open -> ()
    end;
    Lexical.tag buffer ~start:(buffer#get_iter_at_mark (`MARK m1)) ~stop:(buffer#get_iter `INSERT);
    buffer#delete_mark (`MARK m1);
    (*Info.insert_info (!!) buffer elem.Odoc_type.ty_info*)
;;


module Properties =
struct
  let type2 =
    let editor_font = Preferences.preferences#get.Preferences.pref_base_font in
    [`FONT editor_font; `BACKGROUND_FULL_HEIGHT true;
     `PIXELS_ABOVE_LINES 1; `PIXELS_BELOW_LINES 1; `PIXELS_INSIDE_WRAP 0;
     `INDENT 1]

  let line_spacing_small = [`SIZE_POINTS 0.5]
end

  (** create_tags *)
  let create_tags ~(buffer : GText.buffer) =
    let indent = 13 in
    let param_color = Color.name_of_gdk (Preferences.tag_color "label") in
    let editor_font = Preferences.preferences#get.Preferences.pref_base_font in
    let editor_font_family = (GPango.font_description_from_string editor_font) #family in
    let odoc_font = Preferences.preferences#get.Preferences.pref_odoc_font in
    let set_acc_margin tag = Gobject.Property.set tag#as_tag {Gobject.name="accumulative-margin"; conv=Gobject.Data.boolean} true in
    let black = Color.name_of_gdk (Preferences.tag_color "lident") in
    let gray = Color.add_value ~sfact:0.0 black (-.0.2) in
    let default_bg_color =
      if snd Preferences.preferences#get.Preferences.pref_bg_color then begin
        (* "Use theme color" option removed *)
        let color = (*`NAME*) (fst ((Preferences.create_defaults()).Preferences.pref_bg_color)) in
        color;
      end else begin
        let color = (*`NAME*) (fst Preferences.preferences#get.Preferences.pref_bg_color) in
        color;
      end;
    in
    let bgparagraph = Color.add_value ~sfact:0.0 default_bg_color 0.06 in
    let tag_table = new GText.tag_table buffer#tag_table in
    let create_tag id name props =
      match tag_table#lookup name with
        | Some tag -> id, new GText.tag tag
        | _ -> id, buffer#create_tag ~name props;
    in
    let tags = [
      create_tag `BOLD "bold"     [`FONT odoc_font; `WEIGHT `BOLD];
      create_tag `ITALIC "italic" [`FONT odoc_font; `STYLE `ITALIC];
      create_tag `LEFT "left"     [`JUSTIFICATION `LEFT];
      create_tag `RIGHT "right"   [`JUSTIFICATION `RIGHT];
      create_tag `CENTER "center" [`JUSTIFICATION `CENTER];
      create_tag `SANS "sans"     [`FONT editor_font; `FOREGROUND "#5C6585"; (*`FAMILY "Sans"*)];
      create_tag `SUPERSCRIPT "superscript"   [`RISE (GPango.from_pixels 3); `SCALE `SMALL];
      create_tag `SUBSCRIPT "subscript"       [`RISE (-(GPango.from_pixels 3)); `SCALE `SMALL];
      create_tag `PARAGRAPH "paragraph"       [`WEIGHT `BOLD; `PIXELS_ABOVE_LINES 3];
      create_tag `SMALL "small"               [`SCALE `XX_SMALL];
      create_tag `LARGE "large"               [`SCALE `X_LARGE];
      create_tag `TT "tt"                     [`FONT editor_font; (*`FOREGROUND "#5C6585"*)];
      create_tag `TTB "ttb"                   [`FONT editor_font];
      create_tag `TTF "ttf"                   [`FAMILY editor_font_family; `SIZE_POINTS 24.0; `WEIGHT `BOLD(*; `FOREGROUND "#5C6585"*)];
      create_tag `TYPE "type"                 [`FONT editor_font; `PIXELS_ABOVE_LINES 4];
      create_tag `TYPE_COMMENT "type_comment" [`FONT odoc_font; `PIXELS_ABOVE_LINES 0; `INDENT (2 * indent)];
      create_tag `TYPE2 "type2"   Properties.type2;
      create_tag `PARAM "param"   [`LEFT_MARGIN indent; `FONT editor_font; `PIXELS_BELOW_LINES 3; `FOREGROUND param_color; `WEIGHT `BOLD];
      create_tag `PARAM_DESCR "param_descr" [];
      create_tag `LI "li"                                 [`FONT odoc_font; `INDENT indent];
      create_tag `INDENT "indent"                         [`INDENT indent];
      create_tag `LEFT_MARGIN "left_margin"               [`LEFT_MARGIN indent];
      create_tag `LINE_SPACING_SMALL "line_spacing_small" Properties.line_spacing_small;
      (*create_tag `LINE_SPACING_MEDIUM "line_spacing_medium" [`SIZE_POINTS 4.75];*)
      create_tag `LINE_SPACING_BIG "line_spacing_big"     [`SIZE_POINTS 8.];
      create_tag (`TITLE 9) "title9" [`WEIGHT `BOLD; `SIZE_POINTS 13.0; `PIXELS_ABOVE_LINES 21; `PIXELS_BELOW_LINES 13; `FOREGROUND gray];
      create_tag (`TITLE 8) "title8" [`WEIGHT `BOLD; `SIZE_POINTS 14.0; `PIXELS_ABOVE_LINES 21; `PIXELS_BELOW_LINES 13; `FOREGROUND gray];
      create_tag (`TITLE 7) "title7" [`WEIGHT `BOLD; `SIZE_POINTS 15.0; `PIXELS_ABOVE_LINES 21; `PIXELS_BELOW_LINES 13; `FOREGROUND gray];
      create_tag (`TITLE 6) "title6" [`WEIGHT `BOLD; `SIZE_POINTS 19.0; `PIXELS_ABOVE_LINES 13; `PIXELS_BELOW_LINES 13];
      create_tag (`TITLE 5) "title5" [`WEIGHT `BOLD; `SIZE_POINTS 13.0; `PIXELS_ABOVE_LINES 13; `PIXELS_BELOW_LINES 8; `FOREGROUND gray];
      create_tag (`TITLE 4) "title4" [`WEIGHT `BOLD; `SIZE_POINTS 13.5; `PIXELS_ABOVE_LINES 21; `PIXELS_BELOW_LINES 13; `FOREGROUND gray];
      create_tag (`TITLE 3) "title3" [`WEIGHT `BOLD; `SIZE_POINTS 14.0; `PIXELS_ABOVE_LINES 21; `PIXELS_BELOW_LINES 13; `FOREGROUND gray];
      create_tag (`TITLE 2) "title2" [`WEIGHT `BOLD; `SIZE_POINTS 15.0; `PIXELS_ABOVE_LINES 21; `PIXELS_BELOW_LINES 13; `FOREGROUND gray];
      create_tag (`TITLE 1) "title1" [`WEIGHT `BOLD; `SIZE_POINTS 19.0; `PIXELS_ABOVE_LINES 13; `PIXELS_BELOW_LINES 13];
      create_tag (`TITLE 0) "title0" [`WEIGHT `BOLD; `SIZE_POINTS 21.0; `PIXELS_ABOVE_LINES 13; `PIXELS_BELOW_LINES 13(*; `UNDERLINE `SINGLE*)];
    ] in
    let ftag x = (*try*) List.assoc x tags (*with Not_found -> assert false*) in
    let (!!) = ftag in
    List.iter set_acc_margin [!!`PARAM; !!`PARAM_DESCR; !!`LI; !!`LEFT_MARGIN; !!`TYPE; !!`TYPE_COMMENT; !!`TYPE2];
    List.iter (fun t -> Gmisclib.Util.set_tag_paragraph_background t bgparagraph) [!!`TYPE2];
    (* Synchronize with preferences *)
    ignore (Preferences.preferences#connect#changed ~callback:begin fun pref ->
      List.iter begin fun t ->
        let tag = List.assoc t tags in
        tag#set_property (`FONT pref.Preferences.pref_base_font)
      end [`TT; `TTB; `TTF; `TYPE; `TYPE2; `PARAM];
      let tag = List.assoc `TTF tags in
      tag#set_property (`FAMILY (GPango.font_description_from_string pref.Preferences.pref_base_font) #family);
      let tag = List.assoc `TYPE_COMMENT tags in
      tag#set_property (`FONT pref.Preferences.pref_odoc_font);
    end);
    ftag;;

  (** insert *)
  let insert ~(buffer : GText.buffer) ~kind elem =
    let (!!) = create_tags ~buffer in
    let insert_info = Info.insert_info (!!) buffer in
    let [@warning "-4"] _ = "Disable this pattern matching is fragile warning" in
    begin
      match elem with
        | Search.Res_type elem
            when List.mem kind [Oe.Ptype; Oe.Ptype_abstract; Oe.Ptype_variant; Oe.Ptype_record] ->
          insert_type ~buffer (!!) elem;
          Info.insert_info (!!) buffer elem.Odoc_type.ty_info;
        | Search.Res_value elem
          when List.mem kind [Oe.Pvalue; Oe.Pfunc] ->
          insert_info elem.Value.val_info
        | Search.Res_module elem ->
          (*buffer#insert ~tags:[!!`BOLD; !!`LARGE] "Module ";*)
          buffer#insert ~tags:[!!`BOLD; !!`LARGE; !!`TTB] elem.Module.m_name;
          insert_info elem.Module.m_info
        | Search.Res_module_type elem ->
          (*buffer#insert ~tags:[!!`BOLD; !!`LARGE] "Module type ";*)
          buffer#insert ~tags:[!!`BOLD; !!`LARGE; !!`TTB] elem.Module.mt_name;
          insert_info elem.Module.mt_info
        | Search.Res_class elem ->
          insert_info elem.Class.cl_info
        | Search.Res_class_type elem ->
          insert_info elem.Class.clt_info
        | Search.Res_exception elem ->
          insert_info elem.Exception.ex_info
        | Search.Res_attribute elem ->
          insert_info elem.Value.att_value.Value.val_info
        | Search.Res_method elem ->
          insert_info elem.Value.met_value.Value.val_info
        | Search.Res_section (name, text) ->
          buffer#insert ~tags:[!!`ITALIC] name;
          insert_newline ~buffer (!!);
          insert_text buffer text (!!);
        | _ -> ()
    end;
    insert_newline ~buffer (!!);;

  (** insert_full_module *)
  let insert_full_module ~(buffer : GText.buffer) ~colorize ~tag (odoc : Odoc_module.t_module) =
    let with_tag_odoc = Gtk_util.with_tag ~buffer tag in
    let (!!) = create_tags ~buffer in
    let marks = ref [] in
    let insert_elem descr f =
      insert_newline ~buffer (!!);
      let m = buffer#create_mark (buffer#get_iter `INSERT) in
      marks := m :: !marks;
      f();
      let start = buffer#get_iter (`MARK m) in
      colorize ~start ~stop:(buffer#get_iter `INSERT) buffer;
      buffer#insert ~tags:[!!(`LINE_SPACING_SMALL)] "\n\n";
      let iter = buffer#get_iter `INSERT in
      with_tag_odoc begin fun () ->
        match descr with
          | `Info info ->
            Info.insert_info ~newline_before:false (!!) buffer info;
          | `Text text ->
            insert_text buffer (Odoc_info.first_sentence_of_text text) (!!);
      end;
      let nn = if (buffer#get_iter `INSERT)#equal iter then "\n" else "\n\n" in
      buffer#insert ~tags:[!!(`LINE_SPACING_BIG)] nn;
    in
    (* Title and module description *)
    let fix_ocamldoc = ref (Gaux.may_map ~f:(fun x -> String.trim (Odoc_info.string_of_info x)) odoc.Module.m_info) in
    with_tag_odoc begin fun () ->
      (*buffer#insert ~tags:[!!(`TITLE 0)] "Module ";*)
      buffer#insert ~tags:[!!(`TITLE 0); !!`TTF] odoc.Module.m_name;
      Info.insert_info ~newline_before:true (!!) buffer odoc.Module.m_info;
      buffer#insert "\n\n";
    end;
    (*  *)
    let tag_type2 = [!!`TYPE2] in
    List.iter begin fun me ->
      let get_relative = Name.get_relative odoc.Module.m_name in
      (*GtkThread2.sync begin fun () ->*)
        match me with
          | Module.Element_module elem ->
            insert_elem (`Info elem.Module.m_info) begin fun () ->
              buffer#insert ~tags:tag_type2 ("module " ^ (get_relative elem.Module.m_name));
            end
          | Module.Element_module_type elem ->
            insert_elem (`Info elem.Module.mt_info) begin fun () ->
              buffer#insert ~tags:tag_type2 ("module type " ^ (get_relative elem.Module.mt_name));
            end
          | Module.Element_included_module elem ->
            insert_elem (`Info elem.Module.im_info) begin fun () ->
              buffer#insert ~tags:tag_type2 ("include " ^ (get_relative elem.Module.im_name));
            end
          | Module.Element_class elem ->
            let text =
              match elem.Class.cl_info with
                | Some info ->
                  (match info.Odoc_info.i_desc with Some text -> text | _ -> [])
                | _ -> []
            in
            insert_elem (`Text text) begin fun () ->
              buffer#insert ~tags:tag_type2 ("class " ^ (get_relative elem.Class.cl_name));
            end;
          | Module.Element_class_type elem ->
            insert_elem (`Info elem.Class.clt_info) begin fun () ->
              buffer#insert ~tags:tag_type2 ("class type " ^ (get_relative elem.Class.clt_name));
            end
          | Module.Element_value elem ->
            insert_elem (`Info elem.Value.val_info) begin fun () ->
              (*Odoc_info.reset_type_names();*)
              let typ = Odoc_info.string_of_type_expr elem.Value.val_type in
              (*let typ = Str.global_replace (!~~ ((Name.father elem.Value.val_name) ^ ".")) "" typ in*)
              buffer#insert ~tags:tag_type2 (String.concat " " ["val"; (get_relative elem.Value.val_name); ":"; typ]);
            end;
          | Module.Element_exception elem ->
            insert_elem (`Info elem.Exception.ex_info) begin fun () ->
              buffer#insert ~tags:tag_type2 "exception ";
              buffer#insert ~tags:tag_type2 (get_relative elem.Exception.ex_name);
              match elem.Exception.ex_args with
              | Odoc_type.Cstr_tuple [] -> ()
              | Odoc_type.Cstr_tuple core_types ->
                ( buffer#insert ~tags:tag_type2 " of "
                ; Odoc_info.reset_type_names ()
                ; buffer#insert ~tags:tag_type2 (Odoc_info.string_of_type_list " * " core_types)
                )
              | Odoc_type.Cstr_record fields ->
                buffer#insert ~tags:tag_type2 (Odoc_info.string_of_record fields) (* ??? More *)
            end;
          | Module.Element_type elem ->
            insert_elem (`Info elem.Odoc_type.ty_info) begin fun () ->
              Gtk_util.with_tag !!`TYPE2 ~buffer (fun () -> insert_type ~buffer (!!) elem);
            end;
          | Module.Element_module_comment elem ->
            (* This is to fix an ocamldoc bug. *)
            (match !fix_ocamldoc with
              | Some text when String.trim (Odoc_info.string_of_text elem) = text ->
                fix_ocamldoc := None;
              | _ -> with_tag_odoc (fun () -> insert_text buffer elem (!!)));
      (*end ()*)
          (* Since 4.02.0 -- TODO *)
          | Module.Element_type_extension _ -> ()
    end (Module.module_elements odoc);
    (*  *)
    insert_newline ~buffer (!!);
    Gmisclib.Idle.add ~prio:300 begin fun () ->
      List.iter (fun m -> buffer#delete_mark (`MARK m)) !marks
    end
  ;;
end
