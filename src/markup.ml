open Preferences
open Printf

let re_indent = Str.regexp "^[ ]+"
let re_multi_space = Str.regexp " [ ]+"
let re_newlines = Str.regexp "[\n\r]+"

let type_info ?(color=false) =
  if color then Lexical_markup.parse Preferences.preferences#get ?highlights:None
  else Print_type.markup2

class odoc () =
  let code_color = ?? (preferences#get.editor_fg_color_popup) in
  let code_font_name = Preferences.preferences#get.editor_base_font in
  let code_font_family =
    String.sub code_font_name 0 (Option.value (String.rindex_opt code_font_name ' ') ~default:(String.length code_font_name)) in
  let pending_newline = ref false in
  object (self)
    method code_font_family = code_font_family

    method convert info =
      let open Odoc_info in
      let odoc = info_of_string info in
      match odoc.i_desc with
      | None -> ""
      | Some idesc ->
          idesc
          |> List.map self#convert_element
          |> String.concat ""

    method private convert_element element =
      let open Odoc_info in
      let markup_of_elements text = text |> List.map self#convert_element |> String.concat "" in
      begin
        match element with
        | Odoc_info.Raw text ->
            let spc = if !pending_newline then "" else " " in
            pending_newline := false;
            text
            |> Str.global_replace re_newlines " "
            |> Str.global_replace re_multi_space " "
            |> Str.global_replace re_indent spc
            |> Glib.Markup.escape_text
        | Code code ->
            code
            |> Glib.Markup.escape_text
            |> sprintf "<span color='%s' face='%s'>%s</span>" code_color code_font_family
        | CodePre code ->
            code
            |> Glib.Markup.escape_text
            |> sprintf "\n<span color='%s' face='%s'>%s</span>\n" code_color code_font_family
        | Verbatim text ->
            text
            |> Glib.Markup.escape_text
            |> sprintf "<tt>%s</tt>"
        | Bold text ->
            sprintf "<b>%s</b>" (markup_of_elements text)
        | Italic text ->
            sprintf "<i>%s</i>" (markup_of_elements text)
        | Emphasize text ->
            sprintf "<i>%s</i>" (markup_of_elements text)
        | List text ->
            text
            |> List.map (fun t -> t |> List.map self#convert_element |> String.concat "")
            |> String.concat "\n\u{2022}  "
            |> sprintf "\n\u{2022}  %s\n"
        | Enum text ->
            "\n" ^
            (text
             |> List.map markup_of_elements
             |> List.mapi (fun i -> Printf.sprintf "%3d)  %s" (i+1))
             |> String.concat "\n")
        | Newline -> pending_newline := true; "\n" (*"<span color='red'>\u{ebea}</span>"*)
        | Block text ->
            sprintf "<span color='red'>Block %s</span>" (markup_of_elements text)
        | Link (link, text) ->
            sprintf "%s (<tt>%s</tt>)" (markup_of_elements text) link
        | Ref (name, _kind, text) ->
            sprintf "<span color='%s' face='%s'>%s</span>%s" code_color code_font_family name
              (match text with None -> "" | Some text -> (markup_of_elements text))
        | Module_list _ -> ""
        | Index_list -> ""
        | Target (a, b) -> sprintf "<span color='red'>Target %S %S</span>" a b
        | Center text -> sprintf "<span color='red'>Center %S</span>" (markup_of_elements text)
        | Left text -> sprintf "<span color='red'>Left %S</span>" (markup_of_elements text)
        | Right text -> sprintf "<span color='red'>Right %S</span>" (markup_of_elements text)
        | Title (level, _, text) -> sprintf "<span weight='bold'>%s</span>" (markup_of_elements text)
        | Latex text -> sprintf "<tt>%s</tt>" text
        | Superscript text -> sprintf "<sup>%s</sup>" (markup_of_elements text)
        | Subscript text -> sprintf "<sub>%s</sub>" (markup_of_elements text)
        | Custom (a, text) ->
            sprintf "<span color='red'>Custom %S %s</span>" a (markup_of_elements text)
      end
  end
