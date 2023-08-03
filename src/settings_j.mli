(* Auto-generated from "settings.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type 'a color = 'a Settings_t.color = { mutable light: 'a; mutable dark: 'a }

type editor_tag = Settings_t.editor_tag = {
  mutable name: string;
  mutable color: string color;
  mutable weight: int;
  mutable style: [ `NORMAL | `ITALIC ];
  mutable underline: [ `NONE | `SINGLE ];
  mutable scale: float;
  mutable bg_default: bool;
  mutable bg_color: string color
}

type settings = Settings_t.settings = {
  mutable timestamp: float;
  mutable build_parallel: int option;
  mutable build_verbosity: int;
  mutable check_updates: bool;
  mutable detach_message_panes_separately: bool;
  mutable font: string;
  mutable geometry_delayed: bool;
  mutable hmessages_width: int;
  mutable max_view_1_menubar: bool;
  mutable max_view_1_toolbar: bool;
  mutable max_view_1_tabbar: bool;
  mutable max_view_1_messages: bool;
  mutable max_view_1_fullscreen: bool;
  mutable max_view_2_menubar: bool;
  mutable max_view_2_toolbar: bool;
  mutable max_view_2_tabbar: bool;
  mutable max_view_2_messages: bool;
  mutable max_view_2_fullscreen: bool;
  mutable max_view_prefer_fullscreen: bool;
  mutable menubar_buttons: int list;
  mutable odoc_font: string;
  mutable outline_show_types: bool;
  mutable outline_width: int;
  mutable outline_color_types: string color;
  mutable outline_color_nor_bg: string color;
  mutable outline_color_nor_fg: string color;
  mutable outline_color_sel_bg: string color;
  mutable outline_color_sel_fg: string color;
  mutable outline_color_act_bg: string color;
  mutable outline_color_act_fg: string color;
  mutable outline_color_alt_rows: float option;
  mutable output_font: string;
  mutable output_bg_color: string color;
  mutable output_stdin_fg_color: string color;
  mutable output_stdout_fg_color: string color;
  mutable output_err_fg_color: string color;
  mutable output_warn_fg_color: string color;
  mutable program_diff: string;
  mutable program_diff_graphical: string;
  mutable program_pdf_viewer: string;
  mutable remember_window_geometry: bool;
  mutable splashscreen_enabled: bool;
  mutable tab_label_type: int;
  mutable tab_pos: [ `TOP | `RIGHT | `BOTTOM | `LEFT ];
  mutable tab_vertical_text: bool;
  mutable theme: string option;
  mutable theme_is_dark: bool;
  mutable vmessages_height: int;
  mutable editor_annot_type_tooltips_enabled: bool;
  mutable editor_annot_type_tooltips_delay: int;
  mutable editor_annot_type_tooltips_impl: int;
  mutable editor_bak: bool;
  mutable editor_base_font: string;
  mutable editor_bg_color_popup: string color;
  mutable editor_bg_color_theme: bool;
  mutable editor_bg_color_user: string color;
  mutable editor_code_folding_enabled: bool;
  mutable editor_completion_font: string;
  mutable editor_completion_greek_letters: bool;
  mutable editor_completion_decorated: bool;
  mutable editor_completion_opacity: float option;
  mutable editor_current_line_border: bool;
  mutable editor_cursor_aspect_ratio: float;
  mutable editor_custom_templ_filename: string;
  mutable editor_dot_leaders: bool;
  mutable editor_err_gutter: bool;
  mutable editor_err_tooltip: bool;
  mutable editor_err_underline: bool;
  mutable editor_fg_color_popup: string color;
  mutable editor_format_on_save: bool;
  mutable editor_highlight_current_line: bool;
  mutable editor_indent_config: string;
  mutable editor_indent_empty_line: bool;
  mutable editor_indent_lines: (bool * string * string);
  mutable editor_left_margin: int;
  mutable editor_mark_occurrences_enabled: bool;
  mutable editor_mark_occurrences_under_cursor: bool;
  mutable editor_mark_occurrences_bg_color: string color;
  mutable editor_ocamldoc_paragraph_bgcolor_1: string option color;
  mutable editor_ocamldoc_paragraph_bgcolor_2: string option color;
  mutable editor_pixels_lines: (int * int);
  mutable editor_right_margin: int;
  mutable editor_right_margin_color: string color;
  mutable editor_right_margin_visible: bool;
  mutable editor_save_all_bef_comp: bool;
  mutable editor_search_word_at_cursor: bool;
  mutable editor_show_global_gutter: bool;
  mutable editor_show_line_numbers: bool;
  mutable editor_show_whitespace_chars: bool;
  mutable editor_smart_keys_end: int;
  mutable editor_smart_keys_home: int;
  mutable editor_tab_spaces: bool;
  mutable editor_tab_width: int;
  mutable editor_tags: editor_tag list;
  mutable editor_trim_lines: bool;
  mutable editor_wrap: bool
}

val write_color :
  (Buffer.t -> 'a -> unit) ->
  Buffer.t -> 'a color -> unit
  (** Output a JSON value of type {!type:color}. *)

val string_of_color :
  (Buffer.t -> 'a -> unit) ->
  ?len:int -> 'a color -> string
  (** Serialize a value of type {!type:color}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_color :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a color
  (** Input JSON data of type {!type:color}. *)

val color_of_string :
  (Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a) ->
  string -> 'a color
  (** Deserialize JSON data of type {!type:color}. *)

val write_editor_tag :
  Buffer.t -> editor_tag -> unit
  (** Output a JSON value of type {!type:editor_tag}. *)

val string_of_editor_tag :
  ?len:int -> editor_tag -> string
  (** Serialize a value of type {!type:editor_tag}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_editor_tag :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> editor_tag
  (** Input JSON data of type {!type:editor_tag}. *)

val editor_tag_of_string :
  string -> editor_tag
  (** Deserialize JSON data of type {!type:editor_tag}. *)

val write_settings :
  Buffer.t -> settings -> unit
  (** Output a JSON value of type {!type:settings}. *)

val string_of_settings :
  ?len:int -> settings -> string
  (** Serialize a value of type {!type:settings}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_settings :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> settings
  (** Input JSON data of type {!type:settings}. *)

val settings_of_string :
  string -> settings
  (** Deserialize JSON data of type {!type:settings}. *)
