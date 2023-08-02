(* Auto-generated from "settings.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type editor_tag = Settings_t.editor_tag = {
  mutable name: string;
  mutable color: string;
  mutable weight: int;
  mutable style: [ `NORMAL | `ITALIC ];
  mutable underline: [ `NONE | `SINGLE ];
  mutable scale: float;
  mutable bg_default: bool;
  mutable bg_color: string
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
  mutable outline_color_types: string;
  mutable outline_color_nor_bg: string;
  mutable outline_color_nor_fg: string;
  mutable outline_color_sel_bg: string;
  mutable outline_color_sel_fg: string;
  mutable outline_color_act_bg: string;
  mutable outline_color_act_fg: string;
  mutable outline_color_alt_rows: float option;
  mutable output_font: string;
  mutable output_bg_color: string;
  mutable output_stdin_fg_color: string;
  mutable output_stdout_fg_color: string;
  mutable output_err_fg_color: string;
  mutable output_warn_fg_color: string;
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
  mutable editor_bg_color_popup: string;
  mutable editor_bg_color_theme: bool;
  mutable editor_bg_color_user: string;
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
  mutable editor_fg_color_popup: string;
  mutable editor_format_on_save: bool;
  mutable editor_highlight_current_line: bool;
  mutable editor_indent_config: string;
  mutable editor_indent_empty_line: bool;
  mutable editor_indent_lines: (bool * string * string);
  mutable editor_left_margin: int;
  mutable editor_mark_occurrences_enabled: bool;
  mutable editor_mark_occurrences_under_cursor: bool;
  mutable editor_mark_occurrences_bg_color: string;
  mutable editor_ocamldoc_paragraph_bgcolor_1: string option;
  mutable editor_ocamldoc_paragraph_bgcolor_2: string option;
  mutable editor_pixels_lines: (int * int);
  mutable editor_right_margin: int;
  mutable editor_right_margin_color: string;
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
  mutable editor_tags_dark: editor_tag list;
  mutable editor_trim_lines: bool;
  mutable editor_wrap: bool
}

let write_editor_tag : _ -> editor_tag -> _ = (
  fun ob (x : editor_tag) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"color\":";
    (
      Yojson.Safe.write_string
    )
      ob x.color;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"weight\":";
    (
      Yojson.Safe.write_int
    )
      ob x.weight;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"style\":";
    (
      fun ob x ->
        match x with
          | `NORMAL -> Buffer.add_string ob "<\"NORMAL\">"
          | `ITALIC -> Buffer.add_string ob "<\"ITALIC\">"
    )
      ob x.style;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"underline\":";
    (
      fun ob x ->
        match x with
          | `NONE -> Buffer.add_string ob "<\"NONE\">"
          | `SINGLE -> Buffer.add_string ob "<\"SINGLE\">"
    )
      ob x.underline;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"scale\":";
    (
      Yojson.Safe.write_float
    )
      ob x.scale;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"bg_default\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.bg_default;
    if !is_first then
      is_first := false
    else
      Buffer.add_char ob ',';
      Buffer.add_string ob "\"bg_color\":";
    (
      Yojson.Safe.write_string
    )
      ob x.bg_color;
    Buffer.add_char ob '}';
)
let string_of_editor_tag ?(len = 1024) x =
  let ob = Buffer.create len in
  write_editor_tag ob x;
  Buffer.contents ob
let read_editor_tag = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_name = ref (None) in
    let field_color = ref (None) in
    let field_weight = ref (None) in
    let field_style = ref (None) in
    let field_underline = ref (None) in
    let field_scale = ref (None) in
    let field_bg_default = ref (None) in
    let field_bg_color = ref (None) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 5 -> (
                match String.unsafe_get s pos with
                  | 'c' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'r' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      match String.unsafe_get s (pos+1) with
                        | 'c' -> (
                            if String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' then (
                              5
                            )
                            else (
                              -1
                            )
                          )
                        | 't' -> (
                            if String.unsafe_get s (pos+2) = 'y' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' then (
                              3
                            )
                            else (
                              -1
                            )
                          )
                        | _ -> (
                            -1
                          )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 6 -> (
                if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 't' then (
                  2
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'g' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'r' then (
                  7
                )
                else (
                  -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 'e' then (
                  4
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'g' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'd' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 't' then (
                  6
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_name := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 1 ->
            field_color := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | 2 ->
            field_weight := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              )
            );
          | 3 ->
            field_style := (
              Some (
                (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    match Yojson.Safe.start_any_variant p lb with
                      | `Edgy_bracket -> (
                          match Yojson.Safe.read_ident p lb with
                            | "NORMAL" ->
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_gt p lb;
                              `NORMAL
                            | "ITALIC" ->
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_gt p lb;
                              `ITALIC
                            | x ->
                              Atdgen_runtime.Oj_run.invalid_variant_tag p x
                        )
                      | `Double_quote -> (
                          match Yojson.Safe.finish_string p lb with
                            | "NORMAL" ->
                              `NORMAL
                            | "ITALIC" ->
                              `ITALIC
                            | x ->
                              Atdgen_runtime.Oj_run.invalid_variant_tag p x
                        )
                      | `Square_bracket -> (
                          match Atdgen_runtime.Oj_run.read_string p lb with
                            | x ->
                              Atdgen_runtime.Oj_run.invalid_variant_tag p x
                        )
                ) p lb
              )
            );
          | 4 ->
            field_underline := (
              Some (
                (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    match Yojson.Safe.start_any_variant p lb with
                      | `Edgy_bracket -> (
                          match Yojson.Safe.read_ident p lb with
                            | "NONE" ->
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_gt p lb;
                              `NONE
                            | "SINGLE" ->
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_gt p lb;
                              `SINGLE
                            | x ->
                              Atdgen_runtime.Oj_run.invalid_variant_tag p x
                        )
                      | `Double_quote -> (
                          match Yojson.Safe.finish_string p lb with
                            | "NONE" ->
                              `NONE
                            | "SINGLE" ->
                              `SINGLE
                            | x ->
                              Atdgen_runtime.Oj_run.invalid_variant_tag p x
                        )
                      | `Square_bracket -> (
                          match Atdgen_runtime.Oj_run.read_string p lb with
                            | x ->
                              Atdgen_runtime.Oj_run.invalid_variant_tag p x
                        )
                ) p lb
              )
            );
          | 5 ->
            field_scale := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              )
            );
          | 6 ->
            field_bg_default := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              )
            );
          | 7 ->
            field_bg_color := (
              Some (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              )
            );
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'n' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  match String.unsafe_get s pos with
                    | 'c' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'r' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        match String.unsafe_get s (pos+1) with
                          | 'c' -> (
                              if String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' then (
                                5
                              )
                              else (
                                -1
                              )
                            )
                          | 't' -> (
                              if String.unsafe_get s (pos+2) = 'y' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' then (
                                3
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 't' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'g' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'r' then (
                    7
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 'e' then (
                    4
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'g' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'd' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = 't' then (
                    6
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_name := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 1 ->
              field_color := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | 2 ->
              field_weight := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                )
              );
            | 3 ->
              field_style := (
                Some (
                  (
                    fun p lb ->
                      Yojson.Safe.read_space p lb;
                      match Yojson.Safe.start_any_variant p lb with
                        | `Edgy_bracket -> (
                            match Yojson.Safe.read_ident p lb with
                              | "NORMAL" ->
                                Yojson.Safe.read_space p lb;
                                Yojson.Safe.read_gt p lb;
                                `NORMAL
                              | "ITALIC" ->
                                Yojson.Safe.read_space p lb;
                                Yojson.Safe.read_gt p lb;
                                `ITALIC
                              | x ->
                                Atdgen_runtime.Oj_run.invalid_variant_tag p x
                          )
                        | `Double_quote -> (
                            match Yojson.Safe.finish_string p lb with
                              | "NORMAL" ->
                                `NORMAL
                              | "ITALIC" ->
                                `ITALIC
                              | x ->
                                Atdgen_runtime.Oj_run.invalid_variant_tag p x
                          )
                        | `Square_bracket -> (
                            match Atdgen_runtime.Oj_run.read_string p lb with
                              | x ->
                                Atdgen_runtime.Oj_run.invalid_variant_tag p x
                          )
                  ) p lb
                )
              );
            | 4 ->
              field_underline := (
                Some (
                  (
                    fun p lb ->
                      Yojson.Safe.read_space p lb;
                      match Yojson.Safe.start_any_variant p lb with
                        | `Edgy_bracket -> (
                            match Yojson.Safe.read_ident p lb with
                              | "NONE" ->
                                Yojson.Safe.read_space p lb;
                                Yojson.Safe.read_gt p lb;
                                `NONE
                              | "SINGLE" ->
                                Yojson.Safe.read_space p lb;
                                Yojson.Safe.read_gt p lb;
                                `SINGLE
                              | x ->
                                Atdgen_runtime.Oj_run.invalid_variant_tag p x
                          )
                        | `Double_quote -> (
                            match Yojson.Safe.finish_string p lb with
                              | "NONE" ->
                                `NONE
                              | "SINGLE" ->
                                `SINGLE
                              | x ->
                                Atdgen_runtime.Oj_run.invalid_variant_tag p x
                          )
                        | `Square_bracket -> (
                            match Atdgen_runtime.Oj_run.read_string p lb with
                              | x ->
                                Atdgen_runtime.Oj_run.invalid_variant_tag p x
                          )
                  ) p lb
                )
              );
            | 5 ->
              field_scale := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                )
              );
            | 6 ->
              field_bg_default := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                )
              );
            | 7 ->
              field_bg_color := (
                Some (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                )
              );
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            name = (match !field_name with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "name");
            color = (match !field_color with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "color");
            weight = (match !field_weight with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "weight");
            style = (match !field_style with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "style");
            underline = (match !field_underline with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "underline");
            scale = (match !field_scale with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "scale");
            bg_default = (match !field_bg_default with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "bg_default");
            bg_color = (match !field_bg_color with Some x -> x | None -> Atdgen_runtime.Oj_run.missing_field p "bg_color");
          }
         : editor_tag)
      )
)
let editor_tag_of_string s =
  read_editor_tag (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__string_option = (
  Atdgen_runtime.Oj_run.write_option (
    Yojson.Safe.write_string
  )
)
let string_of__string_option ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_option ob x;
  Buffer.contents ob
let read__string_option = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _string_option_of_string s =
  read__string_option (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__int_option = (
  Atdgen_runtime.Oj_run.write_option (
    Yojson.Safe.write_int
  )
)
let string_of__int_option ?(len = 1024) x =
  let ob = Buffer.create len in
  write__int_option ob x;
  Buffer.contents ob
let read__int_option = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _int_option_of_string s =
  read__int_option (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__int_list = (
  Atdgen_runtime.Oj_run.write_list (
    Yojson.Safe.write_int
  )
)
let string_of__int_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__int_list ob x;
  Buffer.contents ob
let read__int_list = (
  Atdgen_runtime.Oj_run.read_list (
    Atdgen_runtime.Oj_run.read_int
  )
)
let _int_list_of_string s =
  read__int_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__float_option = (
  Atdgen_runtime.Oj_run.write_option (
    Yojson.Safe.write_float
  )
)
let string_of__float_option ?(len = 1024) x =
  let ob = Buffer.create len in
  write__float_option ob x;
  Buffer.contents ob
let read__float_option = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "None" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | "Some" ->
              Atdgen_runtime.Oj_run.read_until_field_value p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _float_option_of_string s =
  read__float_option (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__editor_tag_list = (
  Atdgen_runtime.Oj_run.write_list (
    write_editor_tag
  )
)
let string_of__editor_tag_list ?(len = 1024) x =
  let ob = Buffer.create len in
  write__editor_tag_list ob x;
  Buffer.contents ob
let read__editor_tag_list = (
  Atdgen_runtime.Oj_run.read_list (
    read_editor_tag
  )
)
let _editor_tag_list_of_string s =
  read__editor_tag_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_settings : _ -> settings -> _ = (
  fun ob (x : settings) ->
    Buffer.add_char ob '{';
    let is_first = ref true in
    if x.timestamp <> 0.0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"timestamp\":";
      (
        Yojson.Safe.write_float
      )
        ob x.timestamp;
    );
    if x.build_parallel <> Some 0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"build_parallel\":";
      (
        write__int_option
      )
        ob x.build_parallel;
    );
    if x.build_verbosity <> 2 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"build_verbosity\":";
      (
        Yojson.Safe.write_int
      )
        ob x.build_verbosity;
    );
    if x.check_updates <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"check_updates\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.check_updates;
    );
    if x.detach_message_panes_separately <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"detach_message_panes_separately\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.detach_message_panes_separately;
    );
    if x.font <> "Sans 9" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"font\":";
      (
        Yojson.Safe.write_string
      )
        ob x.font;
    );
    if x.geometry_delayed <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"geometry_delayed\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.geometry_delayed;
    );
    if x.hmessages_width <> 1000 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"hmessages_width\":";
      (
        Yojson.Safe.write_int
      )
        ob x.hmessages_width;
    );
    if x.max_view_1_menubar <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"max_view_1_menubar\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.max_view_1_menubar;
    );
    if x.max_view_1_toolbar <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"max_view_1_toolbar\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.max_view_1_toolbar;
    );
    if x.max_view_1_tabbar <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"max_view_1_tabbar\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.max_view_1_tabbar;
    );
    if x.max_view_1_messages <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"max_view_1_messages\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.max_view_1_messages;
    );
    if x.max_view_1_fullscreen <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"max_view_1_fullscreen\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.max_view_1_fullscreen;
    );
    if x.max_view_2_menubar <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"max_view_2_menubar\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.max_view_2_menubar;
    );
    if x.max_view_2_toolbar <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"max_view_2_toolbar\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.max_view_2_toolbar;
    );
    if x.max_view_2_tabbar <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"max_view_2_tabbar\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.max_view_2_tabbar;
    );
    if x.max_view_2_messages <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"max_view_2_messages\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.max_view_2_messages;
    );
    if x.max_view_2_fullscreen <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"max_view_2_fullscreen\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.max_view_2_fullscreen;
    );
    if x.max_view_prefer_fullscreen <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"max_view_prefer_fullscreen\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.max_view_prefer_fullscreen;
    );
    if x.menubar_buttons <> [] then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"menubar_buttons\":";
      (
        write__int_list
      )
        ob x.menubar_buttons;
    );
    if x.odoc_font <> "Serif 9" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"odoc_font\":";
      (
        Yojson.Safe.write_string
      )
        ob x.odoc_font;
    );
    if x.outline_show_types <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"outline_show_types\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.outline_show_types;
    );
    if x.outline_width <> 250 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"outline_width\":";
      (
        Yojson.Safe.write_int
      )
        ob x.outline_width;
    );
    if x.outline_color_types <> "#877033" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"outline_color_types\":";
      (
        Yojson.Safe.write_string
      )
        ob x.outline_color_types;
    );
    if x.outline_color_nor_bg <> "#FFFFFF" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"outline_color_nor_bg\":";
      (
        Yojson.Safe.write_string
      )
        ob x.outline_color_nor_bg;
    );
    if x.outline_color_nor_fg <> "#000000" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"outline_color_nor_fg\":";
      (
        Yojson.Safe.write_string
      )
        ob x.outline_color_nor_fg;
    );
    if x.outline_color_sel_bg <> "#1F80ED" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"outline_color_sel_bg\":";
      (
        Yojson.Safe.write_string
      )
        ob x.outline_color_sel_bg;
    );
    if x.outline_color_sel_fg <> "#FFFFFF" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"outline_color_sel_fg\":";
      (
        Yojson.Safe.write_string
      )
        ob x.outline_color_sel_fg;
    );
    if x.outline_color_act_bg <> "#B1C3D8" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"outline_color_act_bg\":";
      (
        Yojson.Safe.write_string
      )
        ob x.outline_color_act_bg;
    );
    if x.outline_color_act_fg <> "#000000" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"outline_color_act_fg\":";
      (
        Yojson.Safe.write_string
      )
        ob x.outline_color_act_fg;
    );
    if x.outline_color_alt_rows <> None then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"outline_color_alt_rows\":";
      (
        write__float_option
      )
        ob x.outline_color_alt_rows;
    );
    if x.output_font <> "monospace 8" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"output_font\":";
      (
        Yojson.Safe.write_string
      )
        ob x.output_font;
    );
    if x.output_bg_color <> "#FFFFFF" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"output_bg_color\":";
      (
        Yojson.Safe.write_string
      )
        ob x.output_bg_color;
    );
    if x.output_stdin_fg_color <> "#0000FF" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"output_stdin_fg_color\":";
      (
        Yojson.Safe.write_string
      )
        ob x.output_stdin_fg_color;
    );
    if x.output_stdout_fg_color <> "#000000" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"output_stdout_fg_color\":";
      (
        Yojson.Safe.write_string
      )
        ob x.output_stdout_fg_color;
    );
    if x.output_err_fg_color <> "#FF0000" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"output_err_fg_color\":";
      (
        Yojson.Safe.write_string
      )
        ob x.output_err_fg_color;
    );
    if x.output_warn_fg_color <> "darkorange" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"output_warn_fg_color\":";
      (
        Yojson.Safe.write_string
      )
        ob x.output_warn_fg_color;
    );
    if x.program_diff <> "diff" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"program_diff\":";
      (
        Yojson.Safe.write_string
      )
        ob x.program_diff;
    );
    if x.program_diff_graphical <> "" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"program_diff_graphical\":";
      (
        Yojson.Safe.write_string
      )
        ob x.program_diff_graphical;
    );
    if x.program_pdf_viewer <> "" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"program_pdf_viewer\":";
      (
        Yojson.Safe.write_string
      )
        ob x.program_pdf_viewer;
    );
    if x.remember_window_geometry <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"remember_window_geometry\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.remember_window_geometry;
    );
    if x.splashscreen_enabled <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"splashscreen_enabled\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.splashscreen_enabled;
    );
    if x.tab_label_type <> 1 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"tab_label_type\":";
      (
        Yojson.Safe.write_int
      )
        ob x.tab_label_type;
    );
    if x.tab_pos <> `TOP then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"tab_pos\":";
      (
        fun ob x ->
          match x with
            | `TOP -> Buffer.add_string ob "<\"TOP\">"
            | `RIGHT -> Buffer.add_string ob "<\"RIGHT\">"
            | `BOTTOM -> Buffer.add_string ob "<\"BOTTOM\">"
            | `LEFT -> Buffer.add_string ob "<\"LEFT\">"
      )
        ob x.tab_pos;
    );
    if x.tab_vertical_text <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"tab_vertical_text\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.tab_vertical_text;
    );
    if x.theme <> None then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"theme\":";
      (
        write__string_option
      )
        ob x.theme;
    );
    if x.theme_is_dark <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"theme_is_dark\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.theme_is_dark;
    );
    if x.vmessages_height <> 300 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"vmessages_height\":";
      (
        Yojson.Safe.write_int
      )
        ob x.vmessages_height;
    );
    if x.editor_annot_type_tooltips_enabled <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_annot_type_tooltips_enabled\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_annot_type_tooltips_enabled;
    );
    if x.editor_annot_type_tooltips_delay <> 0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_annot_type_tooltips_delay\":";
      (
        Yojson.Safe.write_int
      )
        ob x.editor_annot_type_tooltips_delay;
    );
    if x.editor_annot_type_tooltips_impl <> 0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_annot_type_tooltips_impl\":";
      (
        Yojson.Safe.write_int
      )
        ob x.editor_annot_type_tooltips_impl;
    );
    if x.editor_bak <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_bak\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_bak;
    );
    if x.editor_base_font <> "monospace 9" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_base_font\":";
      (
        Yojson.Safe.write_string
      )
        ob x.editor_base_font;
    );
    if x.editor_bg_color_popup <> "#FFE375" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_bg_color_popup\":";
      (
        Yojson.Safe.write_string
      )
        ob x.editor_bg_color_popup;
    );
    if x.editor_bg_color_theme <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_bg_color_theme\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_bg_color_theme;
    );
    if x.editor_bg_color_user <> "#FFFFFF" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_bg_color_user\":";
      (
        Yojson.Safe.write_string
      )
        ob x.editor_bg_color_user;
    );
    if x.editor_code_folding_enabled <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_code_folding_enabled\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_code_folding_enabled;
    );
    if x.editor_completion_font <> "Sans 8" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_completion_font\":";
      (
        Yojson.Safe.write_string
      )
        ob x.editor_completion_font;
    );
    if x.editor_completion_greek_letters <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_completion_greek_letters\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_completion_greek_letters;
    );
    if x.editor_completion_decorated <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_completion_decorated\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_completion_decorated;
    );
    if x.editor_completion_opacity <> None then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_completion_opacity\":";
      (
        write__float_option
      )
        ob x.editor_completion_opacity;
    );
    if x.editor_current_line_border <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_current_line_border\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_current_line_border;
    );
    if x.editor_cursor_aspect_ratio <> 0.1 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_cursor_aspect_ratio\":";
      (
        Yojson.Safe.write_float
      )
        ob x.editor_cursor_aspect_ratio;
    );
    if x.editor_custom_templ_filename <> "" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_custom_templ_filename\":";
      (
        Yojson.Safe.write_string
      )
        ob x.editor_custom_templ_filename;
    );
    if x.editor_dot_leaders <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_dot_leaders\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_dot_leaders;
    );
    if x.editor_err_gutter <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_err_gutter\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_err_gutter;
    );
    if x.editor_err_tooltip <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_err_tooltip\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_err_tooltip;
    );
    if x.editor_err_underline <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_err_underline\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_err_underline;
    );
    if x.editor_fg_color_popup <> "#000000" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_fg_color_popup\":";
      (
        Yojson.Safe.write_string
      )
        ob x.editor_fg_color_popup;
    );
    if x.editor_format_on_save <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_format_on_save\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_format_on_save;
    );
    if x.editor_highlight_current_line <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_highlight_current_line\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_highlight_current_line;
    );
    if x.editor_indent_config <> "" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_indent_config\":";
      (
        Yojson.Safe.write_string
      )
        ob x.editor_indent_config;
    );
    if x.editor_indent_empty_line <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_indent_empty_line\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_indent_empty_line;
    );
    if x.editor_indent_lines <> (true, "#e6e6e6", "#d0d0d0") then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_indent_lines\":";
      (
        fun ob x ->
          Buffer.add_char ob '(';
          (let x, _, _ = x in
          (
            Yojson.Safe.write_bool
          ) ob x
          );
          Buffer.add_char ob ',';
          (let _, x, _ = x in
          (
            Yojson.Safe.write_string
          ) ob x
          );
          Buffer.add_char ob ',';
          (let _, _, x = x in
          (
            Yojson.Safe.write_string
          ) ob x
          );
          Buffer.add_char ob ')';
      )
        ob x.editor_indent_lines;
    );
    if x.editor_left_margin <> 1 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_left_margin\":";
      (
        Yojson.Safe.write_int
      )
        ob x.editor_left_margin;
    );
    if x.editor_mark_occurrences_enabled <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_mark_occurrences_enabled\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_mark_occurrences_enabled;
    );
    if x.editor_mark_occurrences_under_cursor <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_mark_occurrences_under_cursor\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_mark_occurrences_under_cursor;
    );
    if x.editor_mark_occurrences_bg_color <> "#c8ffc8" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_mark_occurrences_bg_color\":";
      (
        Yojson.Safe.write_string
      )
        ob x.editor_mark_occurrences_bg_color;
    );
    if x.editor_ocamldoc_paragraph_bgcolor_1 <> Some "#FAF7FA" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_ocamldoc_paragraph_bgcolor_1\":";
      (
        write__string_option
      )
        ob x.editor_ocamldoc_paragraph_bgcolor_1;
    );
    if x.editor_ocamldoc_paragraph_bgcolor_2 <> Some "#FAF7FA" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_ocamldoc_paragraph_bgcolor_2\":";
      (
        write__string_option
      )
        ob x.editor_ocamldoc_paragraph_bgcolor_2;
    );
    if x.editor_pixels_lines <> (0, 2) then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_pixels_lines\":";
      (
        fun ob x ->
          Buffer.add_char ob '(';
          (let x, _ = x in
          (
            Yojson.Safe.write_int
          ) ob x
          );
          Buffer.add_char ob ',';
          (let _, x = x in
          (
            Yojson.Safe.write_int
          ) ob x
          );
          Buffer.add_char ob ')';
      )
        ob x.editor_pixels_lines;
    );
    if x.editor_right_margin <> 80 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_right_margin\":";
      (
        Yojson.Safe.write_int
      )
        ob x.editor_right_margin;
    );
    if x.editor_right_margin_color <> "#e0e0e0" then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_right_margin_color\":";
      (
        Yojson.Safe.write_string
      )
        ob x.editor_right_margin_color;
    );
    if x.editor_right_margin_visible <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_right_margin_visible\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_right_margin_visible;
    );
    if x.editor_save_all_bef_comp <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_save_all_bef_comp\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_save_all_bef_comp;
    );
    if x.editor_search_word_at_cursor <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_search_word_at_cursor\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_search_word_at_cursor;
    );
    if x.editor_show_global_gutter <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_show_global_gutter\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_show_global_gutter;
    );
    if x.editor_show_line_numbers <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_show_line_numbers\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_show_line_numbers;
    );
    if x.editor_show_whitespace_chars <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_show_whitespace_chars\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_show_whitespace_chars;
    );
    if x.editor_smart_keys_end <> 0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_smart_keys_end\":";
      (
        Yojson.Safe.write_int
      )
        ob x.editor_smart_keys_end;
    );
    if x.editor_smart_keys_home <> 0 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_smart_keys_home\":";
      (
        Yojson.Safe.write_int
      )
        ob x.editor_smart_keys_home;
    );
    if x.editor_tab_spaces <> true then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_tab_spaces\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_tab_spaces;
    );
    if x.editor_tab_width <> 2 then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_tab_width\":";
      (
        Yojson.Safe.write_int
      )
        ob x.editor_tab_width;
    );
    if x.editor_tags <> [] then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_tags\":";
      (
        write__editor_tag_list
      )
        ob x.editor_tags;
    );
    if x.editor_tags_dark <> [] then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_tags_dark\":";
      (
        write__editor_tag_list
      )
        ob x.editor_tags_dark;
    );
    if x.editor_trim_lines <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_trim_lines\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_trim_lines;
    );
    if x.editor_wrap <> false then (
      if !is_first then
        is_first := false
      else
        Buffer.add_char ob ',';
        Buffer.add_string ob "\"editor_wrap\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.editor_wrap;
    );
    Buffer.add_char ob '}';
)
let string_of_settings ?(len = 1024) x =
  let ob = Buffer.create len in
  write_settings ob x;
  Buffer.contents ob
let read_settings = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_timestamp = ref (0.0) in
    let field_build_parallel = ref (Some 0) in
    let field_build_verbosity = ref (2) in
    let field_check_updates = ref (false) in
    let field_detach_message_panes_separately = ref (false) in
    let field_font = ref ("Sans 9") in
    let field_geometry_delayed = ref (false) in
    let field_hmessages_width = ref (1000) in
    let field_max_view_1_menubar = ref (true) in
    let field_max_view_1_toolbar = ref (false) in
    let field_max_view_1_tabbar = ref (false) in
    let field_max_view_1_messages = ref (false) in
    let field_max_view_1_fullscreen = ref (false) in
    let field_max_view_2_menubar = ref (true) in
    let field_max_view_2_toolbar = ref (true) in
    let field_max_view_2_tabbar = ref (true) in
    let field_max_view_2_messages = ref (true) in
    let field_max_view_2_fullscreen = ref (true) in
    let field_max_view_prefer_fullscreen = ref (true) in
    let field_menubar_buttons = ref ([]) in
    let field_odoc_font = ref ("Serif 9") in
    let field_outline_show_types = ref (true) in
    let field_outline_width = ref (250) in
    let field_outline_color_types = ref ("#877033") in
    let field_outline_color_nor_bg = ref ("#FFFFFF") in
    let field_outline_color_nor_fg = ref ("#000000") in
    let field_outline_color_sel_bg = ref ("#1F80ED") in
    let field_outline_color_sel_fg = ref ("#FFFFFF") in
    let field_outline_color_act_bg = ref ("#B1C3D8") in
    let field_outline_color_act_fg = ref ("#000000") in
    let field_outline_color_alt_rows = ref (None) in
    let field_output_font = ref ("monospace 8") in
    let field_output_bg_color = ref ("#FFFFFF") in
    let field_output_stdin_fg_color = ref ("#0000FF") in
    let field_output_stdout_fg_color = ref ("#000000") in
    let field_output_err_fg_color = ref ("#FF0000") in
    let field_output_warn_fg_color = ref ("darkorange") in
    let field_program_diff = ref ("diff") in
    let field_program_diff_graphical = ref ("") in
    let field_program_pdf_viewer = ref ("") in
    let field_remember_window_geometry = ref (true) in
    let field_splashscreen_enabled = ref (true) in
    let field_tab_label_type = ref (1) in
    let field_tab_pos = ref (`TOP) in
    let field_tab_vertical_text = ref (false) in
    let field_theme = ref (None) in
    let field_theme_is_dark = ref (false) in
    let field_vmessages_height = ref (300) in
    let field_editor_annot_type_tooltips_enabled = ref (false) in
    let field_editor_annot_type_tooltips_delay = ref (0) in
    let field_editor_annot_type_tooltips_impl = ref (0) in
    let field_editor_bak = ref (true) in
    let field_editor_base_font = ref ("monospace 9") in
    let field_editor_bg_color_popup = ref ("#FFE375") in
    let field_editor_bg_color_theme = ref (false) in
    let field_editor_bg_color_user = ref ("#FFFFFF") in
    let field_editor_code_folding_enabled = ref (true) in
    let field_editor_completion_font = ref ("Sans 8") in
    let field_editor_completion_greek_letters = ref (true) in
    let field_editor_completion_decorated = ref (true) in
    let field_editor_completion_opacity = ref (None) in
    let field_editor_current_line_border = ref (true) in
    let field_editor_cursor_aspect_ratio = ref (0.1) in
    let field_editor_custom_templ_filename = ref ("") in
    let field_editor_dot_leaders = ref (false) in
    let field_editor_err_gutter = ref (true) in
    let field_editor_err_tooltip = ref (true) in
    let field_editor_err_underline = ref (true) in
    let field_editor_fg_color_popup = ref ("#000000") in
    let field_editor_format_on_save = ref (false) in
    let field_editor_highlight_current_line = ref (true) in
    let field_editor_indent_config = ref ("") in
    let field_editor_indent_empty_line = ref (true) in
    let field_editor_indent_lines = ref ((true, "#e6e6e6", "#d0d0d0")) in
    let field_editor_left_margin = ref (1) in
    let field_editor_mark_occurrences_enabled = ref (true) in
    let field_editor_mark_occurrences_under_cursor = ref (true) in
    let field_editor_mark_occurrences_bg_color = ref ("#c8ffc8") in
    let field_editor_ocamldoc_paragraph_bgcolor_1 = ref (Some "#FAF7FA") in
    let field_editor_ocamldoc_paragraph_bgcolor_2 = ref (Some "#FAF7FA") in
    let field_editor_pixels_lines = ref ((0, 2)) in
    let field_editor_right_margin = ref (80) in
    let field_editor_right_margin_color = ref ("#e0e0e0") in
    let field_editor_right_margin_visible = ref (true) in
    let field_editor_save_all_bef_comp = ref (true) in
    let field_editor_search_word_at_cursor = ref (true) in
    let field_editor_show_global_gutter = ref (true) in
    let field_editor_show_line_numbers = ref (true) in
    let field_editor_show_whitespace_chars = ref (false) in
    let field_editor_smart_keys_end = ref (0) in
    let field_editor_smart_keys_home = ref (0) in
    let field_editor_tab_spaces = ref (true) in
    let field_editor_tab_width = ref (2) in
    let field_editor_tags = ref ([]) in
    let field_editor_tags_dark = ref ([]) in
    let field_editor_trim_lines = ref (false) in
    let field_editor_wrap = ref (false) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 't' then (
                  5
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'e' then (
                  45
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'p' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 's' then (
                  43
                )
                else (
                  -1
                )
              )
            | 9 -> (
                match String.unsafe_get s pos with
                  | 'o' -> (
                      if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' then (
                        20
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'p' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'b' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'k' then (
                  51
                )
                else (
                  -1
                )
              )
            | 11 -> (
                match String.unsafe_get s pos with
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                        match String.unsafe_get s (pos+7) with
                          | 't' -> (
                              if String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'g' && String.unsafe_get s (pos+10) = 's' then (
                                93
                              )
                              else (
                                -1
                              )
                            )
                          | 'w' -> (
                              if String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'p' then (
                                96
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'o' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'f' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 't' then (
                        31
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 12 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'd' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'f' && String.unsafe_get s (pos+11) = 'f' then (
                  37
                )
                else (
                  -1
                )
              )
            | 13 -> (
                match String.unsafe_get s pos with
                  | 'c' -> (
                      if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'k' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'p' && String.unsafe_get s (pos+8) = 'd' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 's' then (
                        3
                      )
                      else (
                        -1
                      )
                    )
                  | 'o' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'w' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'd' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'h' then (
                        22
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 'k' then (
                        46
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 14 -> (
                match String.unsafe_get s pos with
                  | 'b' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 'l' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'b' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'y' && String.unsafe_get s (pos+12) = 'p' && String.unsafe_get s (pos+13) = 'e' then (
                        42
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 15 -> (
                match String.unsafe_get s pos with
                  | 'b' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'v' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'b' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 's' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'y' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | 'h' -> (
                      if String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'w' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'd' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'h' then (
                        7
                      )
                      else (
                        -1
                      )
                    )
                  | 'm' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'b' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'r' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'b' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 's' then (
                        19
                      )
                      else (
                        -1
                      )
                    )
                  | 'o' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'b' && String.unsafe_get s (pos+8) = 'g' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'l' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'r' then (
                        32
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 16 -> (
                match String.unsafe_get s pos with
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                        match String.unsafe_get s (pos+7) with
                          | 'b' -> (
                              if String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'f' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 't' then (
                                52
                              )
                              else (
                                -1
                              )
                            )
                          | 't' -> (
                              if String.unsafe_get s (pos+8) = 'a' then (
                                match String.unsafe_get s (pos+9) with
                                  | 'b' -> (
                                      if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'w' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'd' && String.unsafe_get s (pos+14) = 't' && String.unsafe_get s (pos+15) = 'h' then (
                                        92
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | 'g' -> (
                                      if String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'd' && String.unsafe_get s (pos+13) = 'a' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = 'k' then (
                                        94
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | _ -> (
                                      -1
                                    )
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'g' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'r' && String.unsafe_get s (pos+7) = 'y' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'y' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'd' then (
                        6
                      )
                      else (
                        -1
                      )
                    )
                  | 'v' -> (
                      if String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'h' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = 'h' && String.unsafe_get s (pos+15) = 't' then (
                        47
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 17 -> (
                match String.unsafe_get s pos with
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                        match String.unsafe_get s (pos+7) with
                          | 'e' -> (
                              if String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'g' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 't' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 'r' then (
                                65
                              )
                              else (
                                -1
                              )
                            )
                          | 't' -> (
                              match String.unsafe_get s (pos+8) with
                                | 'a' -> (
                                    if String.unsafe_get s (pos+9) = 'b' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 's' && String.unsafe_get s (pos+12) = 'p' && String.unsafe_get s (pos+13) = 'a' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 's' then (
                                      91
                                    )
                                    else (
                                      -1
                                    )
                                  )
                                | 'r' -> (
                                    if String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'm' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'l' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 's' then (
                                      95
                                    )
                                    else (
                                      -1
                                    )
                                  )
                                | _ -> (
                                    -1
                                  )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'm' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = '_' then (
                        match String.unsafe_get s (pos+9) with
                          | '1' -> (
                              if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'b' && String.unsafe_get s (pos+14) = 'b' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'r' then (
                                10
                              )
                              else (
                                -1
                              )
                            )
                          | '2' -> (
                              if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'b' && String.unsafe_get s (pos+14) = 'b' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'r' then (
                                15
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'r' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'c' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'x' && String.unsafe_get s (pos+16) = 't' then (
                        44
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 18 -> (
                match String.unsafe_get s pos with
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                        match String.unsafe_get s (pos+7) with
                          | 'd' -> (
                              if String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 'a' && String.unsafe_get s (pos+14) = 'd' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 'r' && String.unsafe_get s (pos+17) = 's' then (
                                64
                              )
                              else (
                                -1
                              )
                            )
                          | 'e' -> (
                              if String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 't' && String.unsafe_get s (pos+16) = 'i' && String.unsafe_get s (pos+17) = 'p' then (
                                66
                              )
                              else (
                                -1
                              )
                            )
                          | 'l' -> (
                              if String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'f' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'm' && String.unsafe_get s (pos+13) = 'a' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = 'g' && String.unsafe_get s (pos+16) = 'i' && String.unsafe_get s (pos+17) = 'n' then (
                                74
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'm' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = '_' then (
                        match String.unsafe_get s (pos+9) with
                          | '1' -> (
                              if String.unsafe_get s (pos+10) = '_' then (
                                match String.unsafe_get s (pos+11) with
                                  | 'm' -> (
                                      if String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 'u' && String.unsafe_get s (pos+15) = 'b' && String.unsafe_get s (pos+16) = 'a' && String.unsafe_get s (pos+17) = 'r' then (
                                        8
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | 't' -> (
                                      if String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'b' && String.unsafe_get s (pos+16) = 'a' && String.unsafe_get s (pos+17) = 'r' then (
                                        9
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | _ -> (
                                      -1
                                    )
                              )
                              else (
                                -1
                              )
                            )
                          | '2' -> (
                              if String.unsafe_get s (pos+10) = '_' then (
                                match String.unsafe_get s (pos+11) with
                                  | 'm' -> (
                                      if String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 'u' && String.unsafe_get s (pos+15) = 'b' && String.unsafe_get s (pos+16) = 'a' && String.unsafe_get s (pos+17) = 'r' then (
                                        13
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | 't' -> (
                                      if String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'b' && String.unsafe_get s (pos+16) = 'a' && String.unsafe_get s (pos+17) = 'r' then (
                                        14
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | _ -> (
                                      -1
                                    )
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'o' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = 'h' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 'w' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'y' && String.unsafe_get s (pos+15) = 'p' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = 's' then (
                        21
                      )
                      else (
                        -1
                      )
                    )
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'p' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'f' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'v' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'w' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = 'r' then (
                        39
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 19 -> (
                match String.unsafe_get s pos with
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                        match String.unsafe_get s (pos+7) with
                          | 'i' -> (
                              if String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'i' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 's' then (
                                73
                              )
                              else (
                                -1
                              )
                            )
                          | 'p' -> (
                              if String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'x' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'i' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 's' then (
                                80
                              )
                              else (
                                -1
                              )
                            )
                          | 'r' -> (
                              if String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'g' && String.unsafe_get s (pos+10) = 'h' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'm' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 'r' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'i' && String.unsafe_get s (pos+18) = 'n' then (
                                81
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'm' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = '_' then (
                        match String.unsafe_get s (pos+9) with
                          | '1' -> (
                              if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'm' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 's' && String.unsafe_get s (pos+14) = 's' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 's' then (
                                11
                              )
                              else (
                                -1
                              )
                            )
                          | '2' -> (
                              if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'm' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 's' && String.unsafe_get s (pos+14) = 's' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 's' then (
                                16
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'o' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' then (
                        match String.unsafe_get s (pos+3) with
                          | 'l' -> (
                              if String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 't' && String.unsafe_get s (pos+15) = 'y' && String.unsafe_get s (pos+16) = 'p' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 's' then (
                                23
                              )
                              else (
                                -1
                              )
                            )
                          | 'p' -> (
                              if String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'f' && String.unsafe_get s (pos+12) = 'g' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'l' && String.unsafe_get s (pos+17) = 'o' && String.unsafe_get s (pos+18) = 'r' then (
                                35
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 20 -> (
                match String.unsafe_get s pos with
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                        match String.unsafe_get s (pos+7) with
                          | 'b' -> (
                              if String.unsafe_get s (pos+8) = 'g' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'l' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'u' && String.unsafe_get s (pos+17) = 's' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'r' then (
                                55
                              )
                              else (
                                -1
                              )
                            )
                          | 'e' -> (
                              if String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 'n' && String.unsafe_get s (pos+13) = 'd' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'r' && String.unsafe_get s (pos+16) = 'l' && String.unsafe_get s (pos+17) = 'i' && String.unsafe_get s (pos+18) = 'n' && String.unsafe_get s (pos+19) = 'e' then (
                                67
                              )
                              else (
                                -1
                              )
                            )
                          | 'i' -> (
                              if String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = 'f' && String.unsafe_get s (pos+18) = 'i' && String.unsafe_get s (pos+19) = 'g' then (
                                71
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'o' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' then (
                        match String.unsafe_get s (pos+3) with
                          | 'l' -> (
                              if String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = '_' then (
                                match String.unsafe_get s (pos+14) with
                                  | 'a' -> (
                                      if String.unsafe_get s (pos+15) = 'c' && String.unsafe_get s (pos+16) = 't' && String.unsafe_get s (pos+17) = '_' then (
                                        match String.unsafe_get s (pos+18) with
                                          | 'b' -> (
                                              if String.unsafe_get s (pos+19) = 'g' then (
                                                28
                                              )
                                              else (
                                                -1
                                              )
                                            )
                                          | 'f' -> (
                                              if String.unsafe_get s (pos+19) = 'g' then (
                                                29
                                              )
                                              else (
                                                -1
                                              )
                                            )
                                          | _ -> (
                                              -1
                                            )
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | 'n' -> (
                                      if String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'r' && String.unsafe_get s (pos+17) = '_' then (
                                        match String.unsafe_get s (pos+18) with
                                          | 'b' -> (
                                              if String.unsafe_get s (pos+19) = 'g' then (
                                                24
                                              )
                                              else (
                                                -1
                                              )
                                            )
                                          | 'f' -> (
                                              if String.unsafe_get s (pos+19) = 'g' then (
                                                25
                                              )
                                              else (
                                                -1
                                              )
                                            )
                                          | _ -> (
                                              -1
                                            )
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | 's' -> (
                                      if String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 'l' && String.unsafe_get s (pos+17) = '_' then (
                                        match String.unsafe_get s (pos+18) with
                                          | 'b' -> (
                                              if String.unsafe_get s (pos+19) = 'g' then (
                                                26
                                              )
                                              else (
                                                -1
                                              )
                                            )
                                          | 'f' -> (
                                              if String.unsafe_get s (pos+19) = 'g' then (
                                                27
                                              )
                                              else (
                                                -1
                                              )
                                            )
                                          | _ -> (
                                              -1
                                            )
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | _ -> (
                                      -1
                                    )
                              )
                              else (
                                -1
                              )
                            )
                          | 'p' -> (
                              if String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'f' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = '_' && String.unsafe_get s (pos+15) = 'c' && String.unsafe_get s (pos+16) = 'o' && String.unsafe_get s (pos+17) = 'l' && String.unsafe_get s (pos+18) = 'o' && String.unsafe_get s (pos+19) = 'r' then (
                                36
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 'p' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'b' && String.unsafe_get s (pos+17) = 'l' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'd' then (
                        41
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 21 -> (
                match String.unsafe_get s pos with
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                        match String.unsafe_get s (pos+7) with
                          | 'b' -> (
                              if String.unsafe_get s (pos+8) = 'g' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'l' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = '_' then (
                                match String.unsafe_get s (pos+16) with
                                  | 'p' -> (
                                      if String.unsafe_get s (pos+17) = 'o' && String.unsafe_get s (pos+18) = 'p' && String.unsafe_get s (pos+19) = 'u' && String.unsafe_get s (pos+20) = 'p' then (
                                        53
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | 't' -> (
                                      if String.unsafe_get s (pos+17) = 'h' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'm' && String.unsafe_get s (pos+20) = 'e' then (
                                        54
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | _ -> (
                                      -1
                                    )
                              )
                              else (
                                -1
                              )
                            )
                          | 'f' -> (
                              match String.unsafe_get s (pos+8) with
                                | 'g' -> (
                                    if String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'l' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'p' && String.unsafe_get s (pos+17) = 'o' && String.unsafe_get s (pos+18) = 'p' && String.unsafe_get s (pos+19) = 'u' && String.unsafe_get s (pos+20) = 'p' then (
                                      68
                                    )
                                    else (
                                      -1
                                    )
                                  )
                                | 'o' -> (
                                    if String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'm' && String.unsafe_get s (pos+11) = 'a' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'o' && String.unsafe_get s (pos+15) = 'n' && String.unsafe_get s (pos+16) = '_' && String.unsafe_get s (pos+17) = 's' && String.unsafe_get s (pos+18) = 'a' && String.unsafe_get s (pos+19) = 'v' && String.unsafe_get s (pos+20) = 'e' then (
                                      69
                                    )
                                    else (
                                      -1
                                    )
                                  )
                                | _ -> (
                                    -1
                                  )
                            )
                          | 's' -> (
                              if String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'k' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'y' && String.unsafe_get s (pos+16) = 's' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'n' && String.unsafe_get s (pos+20) = 'd' then (
                                89
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'm' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = '_' then (
                        match String.unsafe_get s (pos+9) with
                          | '1' -> (
                              if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'f' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'l' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 's' && String.unsafe_get s (pos+16) = 'c' && String.unsafe_get s (pos+17) = 'r' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'e' && String.unsafe_get s (pos+20) = 'n' then (
                                12
                              )
                              else (
                                -1
                              )
                            )
                          | '2' -> (
                              if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'f' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'l' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 's' && String.unsafe_get s (pos+16) = 'c' && String.unsafe_get s (pos+17) = 'r' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'e' && String.unsafe_get s (pos+20) = 'n' then (
                                17
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'o' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'f' && String.unsafe_get s (pos+14) = 'g' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'c' && String.unsafe_get s (pos+17) = 'o' && String.unsafe_get s (pos+18) = 'l' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'r' then (
                        33
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 22 -> (
                match String.unsafe_get s pos with
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                        match String.unsafe_get s (pos+7) with
                          | 'c' -> (
                              if String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'm' && String.unsafe_get s (pos+10) = 'p' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'f' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'n' && String.unsafe_get s (pos+21) = 't' then (
                                57
                              )
                              else (
                                -1
                              )
                            )
                          | 's' -> (
                              if String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'k' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'y' && String.unsafe_get s (pos+16) = 's' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'h' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'm' && String.unsafe_get s (pos+21) = 'e' then (
                                90
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'o' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' then (
                        match String.unsafe_get s (pos+3) with
                          | 'l' -> (
                              if String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 'l' && String.unsafe_get s (pos+16) = 't' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'r' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'w' && String.unsafe_get s (pos+21) = 's' then (
                                30
                              )
                              else (
                                -1
                              )
                            )
                          | 'p' -> (
                              if String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'f' && String.unsafe_get s (pos+15) = 'g' && String.unsafe_get s (pos+16) = '_' && String.unsafe_get s (pos+17) = 'c' && String.unsafe_get s (pos+18) = 'o' && String.unsafe_get s (pos+19) = 'l' && String.unsafe_get s (pos+20) = 'o' && String.unsafe_get s (pos+21) = 'r' then (
                                34
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'd' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'f' && String.unsafe_get s (pos+11) = 'f' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'p' && String.unsafe_get s (pos+17) = 'h' && String.unsafe_get s (pos+18) = 'i' && String.unsafe_get s (pos+19) = 'c' && String.unsafe_get s (pos+20) = 'a' && String.unsafe_get s (pos+21) = 'l' then (
                        38
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 24 -> (
                match String.unsafe_get s pos with
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                        match String.unsafe_get s (pos+7) with
                          | 'i' -> (
                              if String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'm' && String.unsafe_get s (pos+16) = 'p' && String.unsafe_get s (pos+17) = 't' && String.unsafe_get s (pos+18) = 'y' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'l' && String.unsafe_get s (pos+21) = 'i' && String.unsafe_get s (pos+22) = 'n' && String.unsafe_get s (pos+23) = 'e' then (
                                72
                              )
                              else (
                                -1
                              )
                            )
                          | 's' -> (
                              match String.unsafe_get s (pos+8) with
                                | 'a' -> (
                                    if String.unsafe_get s (pos+9) = 'v' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'l' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'b' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 'f' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'o' && String.unsafe_get s (pos+22) = 'm' && String.unsafe_get s (pos+23) = 'p' then (
                                      84
                                    )
                                    else (
                                      -1
                                    )
                                  )
                                | 'h' -> (
                                    if String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'w' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'l' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = '_' && String.unsafe_get s (pos+17) = 'n' && String.unsafe_get s (pos+18) = 'u' && String.unsafe_get s (pos+19) = 'm' && String.unsafe_get s (pos+20) = 'b' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = 'r' && String.unsafe_get s (pos+23) = 's' then (
                                      87
                                    )
                                    else (
                                      -1
                                    )
                                  )
                                | _ -> (
                                    -1
                                  )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'r' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'b' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'w' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = 'd' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'w' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 'o' && String.unsafe_get s (pos+19) = 'm' && String.unsafe_get s (pos+20) = 'e' && String.unsafe_get s (pos+21) = 't' && String.unsafe_get s (pos+22) = 'r' && String.unsafe_get s (pos+23) = 'y' then (
                        40
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 25 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                  match String.unsafe_get s (pos+7) with
                    | 'c' -> (
                        if String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'm' && String.unsafe_get s (pos+10) = 'p' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'o' && String.unsafe_get s (pos+19) = 'p' && String.unsafe_get s (pos+20) = 'a' && String.unsafe_get s (pos+21) = 'c' && String.unsafe_get s (pos+22) = 'i' && String.unsafe_get s (pos+23) = 't' && String.unsafe_get s (pos+24) = 'y' then (
                          60
                        )
                        else (
                          -1
                        )
                      )
                    | 'r' -> (
                        if String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'g' && String.unsafe_get s (pos+10) = 'h' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'm' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 'r' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'i' && String.unsafe_get s (pos+18) = 'n' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'o' && String.unsafe_get s (pos+22) = 'l' && String.unsafe_get s (pos+23) = 'o' && String.unsafe_get s (pos+24) = 'r' then (
                          82
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+8) = 'h' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'w' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'g' && String.unsafe_get s (pos+13) = 'l' && String.unsafe_get s (pos+14) = 'o' && String.unsafe_get s (pos+15) = 'b' && String.unsafe_get s (pos+16) = 'a' && String.unsafe_get s (pos+17) = 'l' && String.unsafe_get s (pos+18) = '_' && String.unsafe_get s (pos+19) = 'g' && String.unsafe_get s (pos+20) = 'u' && String.unsafe_get s (pos+21) = 't' && String.unsafe_get s (pos+22) = 't' && String.unsafe_get s (pos+23) = 'e' && String.unsafe_get s (pos+24) = 'r' then (
                          86
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 26 -> (
                match String.unsafe_get s pos with
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'u' && String.unsafe_get s (pos+9) = 'r' then (
                        match String.unsafe_get s (pos+10) with
                          | 'r' -> (
                              if String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'n' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = '_' && String.unsafe_get s (pos+15) = 'l' && String.unsafe_get s (pos+16) = 'i' && String.unsafe_get s (pos+17) = 'n' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'b' && String.unsafe_get s (pos+21) = 'o' && String.unsafe_get s (pos+22) = 'r' && String.unsafe_get s (pos+23) = 'd' && String.unsafe_get s (pos+24) = 'e' && String.unsafe_get s (pos+25) = 'r' then (
                                61
                              )
                              else (
                                -1
                              )
                            )
                          | 's' -> (
                              if String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 's' && String.unsafe_get s (pos+16) = 'p' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 'c' && String.unsafe_get s (pos+19) = 't' && String.unsafe_get s (pos+20) = '_' && String.unsafe_get s (pos+21) = 'r' && String.unsafe_get s (pos+22) = 'a' && String.unsafe_get s (pos+23) = 't' && String.unsafe_get s (pos+24) = 'i' && String.unsafe_get s (pos+25) = 'o' then (
                                62
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'm' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'p' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'f' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'f' && String.unsafe_get s (pos+17) = 'u' && String.unsafe_get s (pos+18) = 'l' && String.unsafe_get s (pos+19) = 'l' && String.unsafe_get s (pos+20) = 's' && String.unsafe_get s (pos+21) = 'c' && String.unsafe_get s (pos+22) = 'r' && String.unsafe_get s (pos+23) = 'e' && String.unsafe_get s (pos+24) = 'e' && String.unsafe_get s (pos+25) = 'n' then (
                        18
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 27 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                  match String.unsafe_get s (pos+7) with
                    | 'c' -> (
                        if String.unsafe_get s (pos+8) = 'o' then (
                          match String.unsafe_get s (pos+9) with
                            | 'd' -> (
                                if String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'f' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'd' && String.unsafe_get s (pos+16) = 'i' && String.unsafe_get s (pos+17) = 'n' && String.unsafe_get s (pos+18) = 'g' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'e' && String.unsafe_get s (pos+21) = 'n' && String.unsafe_get s (pos+22) = 'a' && String.unsafe_get s (pos+23) = 'b' && String.unsafe_get s (pos+24) = 'l' && String.unsafe_get s (pos+25) = 'e' && String.unsafe_get s (pos+26) = 'd' then (
                                  56
                                )
                                else (
                                  -1
                                )
                              )
                            | 'm' -> (
                                if String.unsafe_get s (pos+10) = 'p' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'd' && String.unsafe_get s (pos+19) = 'e' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'o' && String.unsafe_get s (pos+22) = 'r' && String.unsafe_get s (pos+23) = 'a' && String.unsafe_get s (pos+24) = 't' && String.unsafe_get s (pos+25) = 'e' && String.unsafe_get s (pos+26) = 'd' then (
                                  59
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'r' -> (
                        if String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'g' && String.unsafe_get s (pos+10) = 'h' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'm' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 'r' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'i' && String.unsafe_get s (pos+18) = 'n' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'v' && String.unsafe_get s (pos+21) = 'i' && String.unsafe_get s (pos+22) = 's' && String.unsafe_get s (pos+23) = 'i' && String.unsafe_get s (pos+24) = 'b' && String.unsafe_get s (pos+25) = 'l' && String.unsafe_get s (pos+26) = 'e' then (
                          83
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 28 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                  match String.unsafe_get s (pos+7) with
                    | 'c' -> (
                        if String.unsafe_get s (pos+8) = 'u' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'm' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 't' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 'm' && String.unsafe_get s (pos+17) = 'p' && String.unsafe_get s (pos+18) = 'l' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'f' && String.unsafe_get s (pos+21) = 'i' && String.unsafe_get s (pos+22) = 'l' && String.unsafe_get s (pos+23) = 'e' && String.unsafe_get s (pos+24) = 'n' && String.unsafe_get s (pos+25) = 'a' && String.unsafe_get s (pos+26) = 'm' && String.unsafe_get s (pos+27) = 'e' then (
                          63
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        match String.unsafe_get s (pos+8) with
                          | 'e' -> (
                              if String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'c' && String.unsafe_get s (pos+12) = 'h' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'w' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'r' && String.unsafe_get s (pos+17) = 'd' && String.unsafe_get s (pos+18) = '_' && String.unsafe_get s (pos+19) = 'a' && String.unsafe_get s (pos+20) = 't' && String.unsafe_get s (pos+21) = '_' && String.unsafe_get s (pos+22) = 'c' && String.unsafe_get s (pos+23) = 'u' && String.unsafe_get s (pos+24) = 'r' && String.unsafe_get s (pos+25) = 's' && String.unsafe_get s (pos+26) = 'o' && String.unsafe_get s (pos+27) = 'r' then (
                                85
                              )
                              else (
                                -1
                              )
                            )
                          | 'h' -> (
                              if String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'w' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'w' && String.unsafe_get s (pos+13) = 'h' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 't' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = 's' && String.unsafe_get s (pos+18) = 'p' && String.unsafe_get s (pos+19) = 'a' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = '_' && String.unsafe_get s (pos+23) = 'c' && String.unsafe_get s (pos+24) = 'h' && String.unsafe_get s (pos+25) = 'a' && String.unsafe_get s (pos+26) = 'r' && String.unsafe_get s (pos+27) = 's' then (
                                88
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 29 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'h' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'g' && String.unsafe_get s (pos+10) = 'h' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = 'h' && String.unsafe_get s (pos+15) = 't' && String.unsafe_get s (pos+16) = '_' && String.unsafe_get s (pos+17) = 'c' && String.unsafe_get s (pos+18) = 'u' && String.unsafe_get s (pos+19) = 'r' && String.unsafe_get s (pos+20) = 'r' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = 'n' && String.unsafe_get s (pos+23) = 't' && String.unsafe_get s (pos+24) = '_' && String.unsafe_get s (pos+25) = 'l' && String.unsafe_get s (pos+26) = 'i' && String.unsafe_get s (pos+27) = 'n' && String.unsafe_get s (pos+28) = 'e' then (
                  70
                )
                else (
                  -1
                )
              )
            | 31 -> (
                match String.unsafe_get s pos with
                  | 'd' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 'a' && String.unsafe_get s (pos+12) = 'g' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = '_' && String.unsafe_get s (pos+15) = 'p' && String.unsafe_get s (pos+16) = 'a' && String.unsafe_get s (pos+17) = 'n' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 's' && String.unsafe_get s (pos+20) = '_' && String.unsafe_get s (pos+21) = 's' && String.unsafe_get s (pos+22) = 'e' && String.unsafe_get s (pos+23) = 'p' && String.unsafe_get s (pos+24) = 'a' && String.unsafe_get s (pos+25) = 'r' && String.unsafe_get s (pos+26) = 'a' && String.unsafe_get s (pos+27) = 't' && String.unsafe_get s (pos+28) = 'e' && String.unsafe_get s (pos+29) = 'l' && String.unsafe_get s (pos+30) = 'y' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                        match String.unsafe_get s (pos+7) with
                          | 'a' -> (
                              if String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'y' && String.unsafe_get s (pos+15) = 'p' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 't' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'o' && String.unsafe_get s (pos+21) = 'l' && String.unsafe_get s (pos+22) = 't' && String.unsafe_get s (pos+23) = 'i' && String.unsafe_get s (pos+24) = 'p' && String.unsafe_get s (pos+25) = 's' && String.unsafe_get s (pos+26) = '_' && String.unsafe_get s (pos+27) = 'i' && String.unsafe_get s (pos+28) = 'm' && String.unsafe_get s (pos+29) = 'p' && String.unsafe_get s (pos+30) = 'l' then (
                                50
                              )
                              else (
                                -1
                              )
                            )
                          | 'c' -> (
                              if String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'm' && String.unsafe_get s (pos+10) = 'p' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'g' && String.unsafe_get s (pos+19) = 'r' && String.unsafe_get s (pos+20) = 'e' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = 'k' && String.unsafe_get s (pos+23) = '_' && String.unsafe_get s (pos+24) = 'l' && String.unsafe_get s (pos+25) = 'e' && String.unsafe_get s (pos+26) = 't' && String.unsafe_get s (pos+27) = 't' && String.unsafe_get s (pos+28) = 'e' && String.unsafe_get s (pos+29) = 'r' && String.unsafe_get s (pos+30) = 's' then (
                                58
                              )
                              else (
                                -1
                              )
                            )
                          | 'm' -> (
                              if String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'k' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'c' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'u' && String.unsafe_get s (pos+16) = 'r' && String.unsafe_get s (pos+17) = 'r' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'n' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = 's' && String.unsafe_get s (pos+23) = '_' && String.unsafe_get s (pos+24) = 'e' && String.unsafe_get s (pos+25) = 'n' && String.unsafe_get s (pos+26) = 'a' && String.unsafe_get s (pos+27) = 'b' && String.unsafe_get s (pos+28) = 'l' && String.unsafe_get s (pos+29) = 'e' && String.unsafe_get s (pos+30) = 'd' then (
                                75
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 32 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                  match String.unsafe_get s (pos+7) with
                    | 'a' -> (
                        if String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'y' && String.unsafe_get s (pos+15) = 'p' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 't' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'o' && String.unsafe_get s (pos+21) = 'l' && String.unsafe_get s (pos+22) = 't' && String.unsafe_get s (pos+23) = 'i' && String.unsafe_get s (pos+24) = 'p' && String.unsafe_get s (pos+25) = 's' && String.unsafe_get s (pos+26) = '_' && String.unsafe_get s (pos+27) = 'd' && String.unsafe_get s (pos+28) = 'e' && String.unsafe_get s (pos+29) = 'l' && String.unsafe_get s (pos+30) = 'a' && String.unsafe_get s (pos+31) = 'y' then (
                          49
                        )
                        else (
                          -1
                        )
                      )
                    | 'm' -> (
                        if String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'k' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'c' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'u' && String.unsafe_get s (pos+16) = 'r' && String.unsafe_get s (pos+17) = 'r' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'n' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = 's' && String.unsafe_get s (pos+23) = '_' && String.unsafe_get s (pos+24) = 'b' && String.unsafe_get s (pos+25) = 'g' && String.unsafe_get s (pos+26) = '_' && String.unsafe_get s (pos+27) = 'c' && String.unsafe_get s (pos+28) = 'o' && String.unsafe_get s (pos+29) = 'l' && String.unsafe_get s (pos+30) = 'o' && String.unsafe_get s (pos+31) = 'r' then (
                          77
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 34 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'y' && String.unsafe_get s (pos+15) = 'p' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 't' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'o' && String.unsafe_get s (pos+21) = 'l' && String.unsafe_get s (pos+22) = 't' && String.unsafe_get s (pos+23) = 'i' && String.unsafe_get s (pos+24) = 'p' && String.unsafe_get s (pos+25) = 's' && String.unsafe_get s (pos+26) = '_' && String.unsafe_get s (pos+27) = 'e' && String.unsafe_get s (pos+28) = 'n' && String.unsafe_get s (pos+29) = 'a' && String.unsafe_get s (pos+30) = 'b' && String.unsafe_get s (pos+31) = 'l' && String.unsafe_get s (pos+32) = 'e' && String.unsafe_get s (pos+33) = 'd' then (
                  48
                )
                else (
                  -1
                )
              )
            | 35 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'm' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'd' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'p' && String.unsafe_get s (pos+17) = 'a' && String.unsafe_get s (pos+18) = 'r' && String.unsafe_get s (pos+19) = 'a' && String.unsafe_get s (pos+20) = 'g' && String.unsafe_get s (pos+21) = 'r' && String.unsafe_get s (pos+22) = 'a' && String.unsafe_get s (pos+23) = 'p' && String.unsafe_get s (pos+24) = 'h' && String.unsafe_get s (pos+25) = '_' && String.unsafe_get s (pos+26) = 'b' && String.unsafe_get s (pos+27) = 'g' && String.unsafe_get s (pos+28) = 'c' && String.unsafe_get s (pos+29) = 'o' && String.unsafe_get s (pos+30) = 'l' && String.unsafe_get s (pos+31) = 'o' && String.unsafe_get s (pos+32) = 'r' && String.unsafe_get s (pos+33) = '_' then (
                  match String.unsafe_get s (pos+34) with
                    | '1' -> (
                        78
                      )
                    | '2' -> (
                        79
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 36 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'k' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'c' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'u' && String.unsafe_get s (pos+16) = 'r' && String.unsafe_get s (pos+17) = 'r' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'n' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = 's' && String.unsafe_get s (pos+23) = '_' && String.unsafe_get s (pos+24) = 'u' && String.unsafe_get s (pos+25) = 'n' && String.unsafe_get s (pos+26) = 'd' && String.unsafe_get s (pos+27) = 'e' && String.unsafe_get s (pos+28) = 'r' && String.unsafe_get s (pos+29) = '_' && String.unsafe_get s (pos+30) = 'c' && String.unsafe_get s (pos+31) = 'u' && String.unsafe_get s (pos+32) = 'r' && String.unsafe_get s (pos+33) = 's' && String.unsafe_get s (pos+34) = 'o' && String.unsafe_get s (pos+35) = 'r' then (
                  76
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_timestamp := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_build_parallel := (
                (
                  read__int_option
                ) p lb
              );
            )
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_build_verbosity := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_check_updates := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 4 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_detach_message_panes_separately := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 5 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_font := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 6 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_geometry_delayed := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 7 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_hmessages_width := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 8 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_max_view_1_menubar := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 9 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_max_view_1_toolbar := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 10 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_max_view_1_tabbar := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 11 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_max_view_1_messages := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 12 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_max_view_1_fullscreen := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 13 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_max_view_2_menubar := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 14 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_max_view_2_toolbar := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 15 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_max_view_2_tabbar := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 16 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_max_view_2_messages := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 17 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_max_view_2_fullscreen := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 18 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_max_view_prefer_fullscreen := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 19 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_menubar_buttons := (
                (
                  read__int_list
                ) p lb
              );
            )
          | 20 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_odoc_font := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 21 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_outline_show_types := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 22 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_outline_width := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 23 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_outline_color_types := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 24 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_outline_color_nor_bg := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 25 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_outline_color_nor_fg := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 26 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_outline_color_sel_bg := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 27 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_outline_color_sel_fg := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 28 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_outline_color_act_bg := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 29 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_outline_color_act_fg := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 30 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_outline_color_alt_rows := (
                (
                  read__float_option
                ) p lb
              );
            )
          | 31 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_output_font := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 32 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_output_bg_color := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 33 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_output_stdin_fg_color := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 34 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_output_stdout_fg_color := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 35 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_output_err_fg_color := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 36 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_output_warn_fg_color := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 37 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_program_diff := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 38 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_program_diff_graphical := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 39 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_program_pdf_viewer := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 40 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_remember_window_geometry := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 41 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_splashscreen_enabled := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 42 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_tab_label_type := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 43 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_tab_pos := (
                (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    match Yojson.Safe.start_any_variant p lb with
                      | `Edgy_bracket -> (
                          match Yojson.Safe.read_ident p lb with
                            | "TOP" ->
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_gt p lb;
                              `TOP
                            | "RIGHT" ->
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_gt p lb;
                              `RIGHT
                            | "BOTTOM" ->
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_gt p lb;
                              `BOTTOM
                            | "LEFT" ->
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_gt p lb;
                              `LEFT
                            | x ->
                              Atdgen_runtime.Oj_run.invalid_variant_tag p x
                        )
                      | `Double_quote -> (
                          match Yojson.Safe.finish_string p lb with
                            | "TOP" ->
                              `TOP
                            | "RIGHT" ->
                              `RIGHT
                            | "BOTTOM" ->
                              `BOTTOM
                            | "LEFT" ->
                              `LEFT
                            | x ->
                              Atdgen_runtime.Oj_run.invalid_variant_tag p x
                        )
                      | `Square_bracket -> (
                          match Atdgen_runtime.Oj_run.read_string p lb with
                            | x ->
                              Atdgen_runtime.Oj_run.invalid_variant_tag p x
                        )
                ) p lb
              );
            )
          | 44 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_tab_vertical_text := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 45 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_theme := (
                (
                  read__string_option
                ) p lb
              );
            )
          | 46 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_theme_is_dark := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 47 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_vmessages_height := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 48 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_annot_type_tooltips_enabled := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 49 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_annot_type_tooltips_delay := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 50 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_annot_type_tooltips_impl := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 51 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_bak := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 52 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_base_font := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 53 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_bg_color_popup := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 54 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_bg_color_theme := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 55 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_bg_color_user := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 56 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_code_folding_enabled := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 57 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_completion_font := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 58 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_completion_greek_letters := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 59 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_completion_decorated := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 60 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_completion_opacity := (
                (
                  read__float_option
                ) p lb
              );
            )
          | 61 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_current_line_border := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 62 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_cursor_aspect_ratio := (
                (
                  Atdgen_runtime.Oj_run.read_number
                ) p lb
              );
            )
          | 63 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_custom_templ_filename := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 64 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_dot_leaders := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 65 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_err_gutter := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 66 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_err_tooltip := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 67 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_err_underline := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 68 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_fg_color_popup := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 69 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_format_on_save := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 70 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_highlight_current_line := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 71 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_indent_config := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 72 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_indent_empty_line := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 73 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_indent_lines := (
                (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_bool
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_string
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x2 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_string
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1, x2)
                    with Yojson.End_of_tuple ->
                      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1; 2 ]);
                ) p lb
              );
            )
          | 74 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_left_margin := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 75 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_mark_occurrences_enabled := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 76 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_mark_occurrences_under_cursor := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 77 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_mark_occurrences_bg_color := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 78 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_ocamldoc_paragraph_bgcolor_1 := (
                (
                  read__string_option
                ) p lb
              );
            )
          | 79 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_ocamldoc_paragraph_bgcolor_2 := (
                (
                  read__string_option
                ) p lb
              );
            )
          | 80 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_pixels_lines := (
                (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            Atdgen_runtime.Oj_run.read_int
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              );
            )
          | 81 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_right_margin := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 82 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_right_margin_color := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
            )
          | 83 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_right_margin_visible := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 84 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_save_all_bef_comp := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 85 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_search_word_at_cursor := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 86 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_show_global_gutter := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 87 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_show_line_numbers := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 88 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_show_whitespace_chars := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 89 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_smart_keys_end := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 90 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_smart_keys_home := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 91 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_tab_spaces := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 92 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_tab_width := (
                (
                  Atdgen_runtime.Oj_run.read_int
                ) p lb
              );
            )
          | 93 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_tags := (
                (
                  read__editor_tag_list
                ) p lb
              );
            )
          | 94 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_tags_dark := (
                (
                  read__editor_tag_list
                ) p lb
              );
            )
          | 95 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_trim_lines := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | 96 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_editor_wrap := (
                (
                  Atdgen_runtime.Oj_run.read_bool
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg (Printf.sprintf "out-of-bounds substring position or length: string = %S, requested position = %i, requested length = %i" s pos len);
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 't' then (
                    5
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'e' then (
                    45
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'p' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 's' then (
                    43
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  match String.unsafe_get s pos with
                    | 'o' -> (
                        if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' then (
                          20
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'p' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'b' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'k' then (
                    51
                  )
                  else (
                    -1
                  )
                )
              | 11 -> (
                  match String.unsafe_get s pos with
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                          match String.unsafe_get s (pos+7) with
                            | 't' -> (
                                if String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'g' && String.unsafe_get s (pos+10) = 's' then (
                                  93
                                )
                                else (
                                  -1
                                )
                              )
                            | 'w' -> (
                                if String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'p' then (
                                  96
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'o' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'f' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 't' then (
                          31
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 12 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'd' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'f' && String.unsafe_get s (pos+11) = 'f' then (
                    37
                  )
                  else (
                    -1
                  )
                )
              | 13 -> (
                  match String.unsafe_get s pos with
                    | 'c' -> (
                        if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'k' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'p' && String.unsafe_get s (pos+8) = 'd' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 's' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | 'o' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'w' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'd' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'h' then (
                          22
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 'k' then (
                          46
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 14 -> (
                  match String.unsafe_get s pos with
                    | 'b' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 'l' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'b' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'l' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'y' && String.unsafe_get s (pos+12) = 'p' && String.unsafe_get s (pos+13) = 'e' then (
                          42
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 15 -> (
                  match String.unsafe_get s pos with
                    | 'b' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'v' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'b' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 's' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'y' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | 'h' -> (
                        if String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'w' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'd' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'h' then (
                          7
                        )
                        else (
                          -1
                        )
                      )
                    | 'm' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'b' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'r' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'b' && String.unsafe_get s (pos+9) = 'u' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 's' then (
                          19
                        )
                        else (
                          -1
                        )
                      )
                    | 'o' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'b' && String.unsafe_get s (pos+8) = 'g' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'l' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'r' then (
                          32
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 16 -> (
                  match String.unsafe_get s pos with
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                          match String.unsafe_get s (pos+7) with
                            | 'b' -> (
                                if String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'f' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 't' then (
                                  52
                                )
                                else (
                                  -1
                                )
                              )
                            | 't' -> (
                                if String.unsafe_get s (pos+8) = 'a' then (
                                  match String.unsafe_get s (pos+9) with
                                    | 'b' -> (
                                        if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'w' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'd' && String.unsafe_get s (pos+14) = 't' && String.unsafe_get s (pos+15) = 'h' then (
                                          92
                                        )
                                        else (
                                          -1
                                        )
                                      )
                                    | 'g' -> (
                                        if String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'd' && String.unsafe_get s (pos+13) = 'a' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = 'k' then (
                                          94
                                        )
                                        else (
                                          -1
                                        )
                                      )
                                    | _ -> (
                                        -1
                                      )
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'g' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'r' && String.unsafe_get s (pos+7) = 'y' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'y' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'd' then (
                          6
                        )
                        else (
                          -1
                        )
                      )
                    | 'v' -> (
                        if String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'h' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = 'h' && String.unsafe_get s (pos+15) = 't' then (
                          47
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 17 -> (
                  match String.unsafe_get s pos with
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                          match String.unsafe_get s (pos+7) with
                            | 'e' -> (
                                if String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'g' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 't' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 'r' then (
                                  65
                                )
                                else (
                                  -1
                                )
                              )
                            | 't' -> (
                                match String.unsafe_get s (pos+8) with
                                  | 'a' -> (
                                      if String.unsafe_get s (pos+9) = 'b' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 's' && String.unsafe_get s (pos+12) = 'p' && String.unsafe_get s (pos+13) = 'a' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 's' then (
                                        91
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | 'r' -> (
                                      if String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'm' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'l' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 's' then (
                                        95
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | _ -> (
                                      -1
                                    )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'm' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = '_' then (
                          match String.unsafe_get s (pos+9) with
                            | '1' -> (
                                if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'b' && String.unsafe_get s (pos+14) = 'b' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'r' then (
                                  10
                                )
                                else (
                                  -1
                                )
                              )
                            | '2' -> (
                                if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'b' && String.unsafe_get s (pos+14) = 'b' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'r' then (
                                  15
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'r' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'c' && String.unsafe_get s (pos+10) = 'a' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'x' && String.unsafe_get s (pos+16) = 't' then (
                          44
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 18 -> (
                  match String.unsafe_get s pos with
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                          match String.unsafe_get s (pos+7) with
                            | 'd' -> (
                                if String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 'a' && String.unsafe_get s (pos+14) = 'd' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 'r' && String.unsafe_get s (pos+17) = 's' then (
                                  64
                                )
                                else (
                                  -1
                                )
                              )
                            | 'e' -> (
                                if String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 't' && String.unsafe_get s (pos+16) = 'i' && String.unsafe_get s (pos+17) = 'p' then (
                                  66
                                )
                                else (
                                  -1
                                )
                              )
                            | 'l' -> (
                                if String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'f' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'm' && String.unsafe_get s (pos+13) = 'a' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = 'g' && String.unsafe_get s (pos+16) = 'i' && String.unsafe_get s (pos+17) = 'n' then (
                                  74
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'm' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = '_' then (
                          match String.unsafe_get s (pos+9) with
                            | '1' -> (
                                if String.unsafe_get s (pos+10) = '_' then (
                                  match String.unsafe_get s (pos+11) with
                                    | 'm' -> (
                                        if String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 'u' && String.unsafe_get s (pos+15) = 'b' && String.unsafe_get s (pos+16) = 'a' && String.unsafe_get s (pos+17) = 'r' then (
                                          8
                                        )
                                        else (
                                          -1
                                        )
                                      )
                                    | 't' -> (
                                        if String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'b' && String.unsafe_get s (pos+16) = 'a' && String.unsafe_get s (pos+17) = 'r' then (
                                          9
                                        )
                                        else (
                                          -1
                                        )
                                      )
                                    | _ -> (
                                        -1
                                      )
                                )
                                else (
                                  -1
                                )
                              )
                            | '2' -> (
                                if String.unsafe_get s (pos+10) = '_' then (
                                  match String.unsafe_get s (pos+11) with
                                    | 'm' -> (
                                        if String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 'u' && String.unsafe_get s (pos+15) = 'b' && String.unsafe_get s (pos+16) = 'a' && String.unsafe_get s (pos+17) = 'r' then (
                                          13
                                        )
                                        else (
                                          -1
                                        )
                                      )
                                    | 't' -> (
                                        if String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'b' && String.unsafe_get s (pos+16) = 'a' && String.unsafe_get s (pos+17) = 'r' then (
                                          14
                                        )
                                        else (
                                          -1
                                        )
                                      )
                                    | _ -> (
                                        -1
                                      )
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'o' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = 'h' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 'w' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'y' && String.unsafe_get s (pos+15) = 'p' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = 's' then (
                          21
                        )
                        else (
                          -1
                        )
                      )
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'p' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'f' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'v' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'w' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = 'r' then (
                          39
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 19 -> (
                  match String.unsafe_get s pos with
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                          match String.unsafe_get s (pos+7) with
                            | 'i' -> (
                                if String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'i' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 's' then (
                                  73
                                )
                                else (
                                  -1
                                )
                              )
                            | 'p' -> (
                                if String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'x' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'i' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 's' then (
                                  80
                                )
                                else (
                                  -1
                                )
                              )
                            | 'r' -> (
                                if String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'g' && String.unsafe_get s (pos+10) = 'h' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'm' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 'r' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'i' && String.unsafe_get s (pos+18) = 'n' then (
                                  81
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'm' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = '_' then (
                          match String.unsafe_get s (pos+9) with
                            | '1' -> (
                                if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'm' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 's' && String.unsafe_get s (pos+14) = 's' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 's' then (
                                  11
                                )
                                else (
                                  -1
                                )
                              )
                            | '2' -> (
                                if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'm' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 's' && String.unsafe_get s (pos+14) = 's' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 's' then (
                                  16
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'o' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' then (
                          match String.unsafe_get s (pos+3) with
                            | 'l' -> (
                                if String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 't' && String.unsafe_get s (pos+15) = 'y' && String.unsafe_get s (pos+16) = 'p' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 's' then (
                                  23
                                )
                                else (
                                  -1
                                )
                              )
                            | 'p' -> (
                                if String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'f' && String.unsafe_get s (pos+12) = 'g' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'l' && String.unsafe_get s (pos+17) = 'o' && String.unsafe_get s (pos+18) = 'r' then (
                                  35
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 20 -> (
                  match String.unsafe_get s pos with
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                          match String.unsafe_get s (pos+7) with
                            | 'b' -> (
                                if String.unsafe_get s (pos+8) = 'g' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'l' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'u' && String.unsafe_get s (pos+17) = 's' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'r' then (
                                  55
                                )
                                else (
                                  -1
                                )
                              )
                            | 'e' -> (
                                if String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 'n' && String.unsafe_get s (pos+13) = 'd' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'r' && String.unsafe_get s (pos+16) = 'l' && String.unsafe_get s (pos+17) = 'i' && String.unsafe_get s (pos+18) = 'n' && String.unsafe_get s (pos+19) = 'e' then (
                                  67
                                )
                                else (
                                  -1
                                )
                              )
                            | 'i' -> (
                                if String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = 'f' && String.unsafe_get s (pos+18) = 'i' && String.unsafe_get s (pos+19) = 'g' then (
                                  71
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'o' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' then (
                          match String.unsafe_get s (pos+3) with
                            | 'l' -> (
                                if String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = '_' then (
                                  match String.unsafe_get s (pos+14) with
                                    | 'a' -> (
                                        if String.unsafe_get s (pos+15) = 'c' && String.unsafe_get s (pos+16) = 't' && String.unsafe_get s (pos+17) = '_' then (
                                          match String.unsafe_get s (pos+18) with
                                            | 'b' -> (
                                                if String.unsafe_get s (pos+19) = 'g' then (
                                                  28
                                                )
                                                else (
                                                  -1
                                                )
                                              )
                                            | 'f' -> (
                                                if String.unsafe_get s (pos+19) = 'g' then (
                                                  29
                                                )
                                                else (
                                                  -1
                                                )
                                              )
                                            | _ -> (
                                                -1
                                              )
                                        )
                                        else (
                                          -1
                                        )
                                      )
                                    | 'n' -> (
                                        if String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'r' && String.unsafe_get s (pos+17) = '_' then (
                                          match String.unsafe_get s (pos+18) with
                                            | 'b' -> (
                                                if String.unsafe_get s (pos+19) = 'g' then (
                                                  24
                                                )
                                                else (
                                                  -1
                                                )
                                              )
                                            | 'f' -> (
                                                if String.unsafe_get s (pos+19) = 'g' then (
                                                  25
                                                )
                                                else (
                                                  -1
                                                )
                                              )
                                            | _ -> (
                                                -1
                                              )
                                        )
                                        else (
                                          -1
                                        )
                                      )
                                    | 's' -> (
                                        if String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 'l' && String.unsafe_get s (pos+17) = '_' then (
                                          match String.unsafe_get s (pos+18) with
                                            | 'b' -> (
                                                if String.unsafe_get s (pos+19) = 'g' then (
                                                  26
                                                )
                                                else (
                                                  -1
                                                )
                                              )
                                            | 'f' -> (
                                                if String.unsafe_get s (pos+19) = 'g' then (
                                                  27
                                                )
                                                else (
                                                  -1
                                                )
                                              )
                                            | _ -> (
                                                -1
                                              )
                                        )
                                        else (
                                          -1
                                        )
                                      )
                                    | _ -> (
                                        -1
                                      )
                                )
                                else (
                                  -1
                                )
                              )
                            | 'p' -> (
                                if String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'n' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'f' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = '_' && String.unsafe_get s (pos+15) = 'c' && String.unsafe_get s (pos+16) = 'o' && String.unsafe_get s (pos+17) = 'l' && String.unsafe_get s (pos+18) = 'o' && String.unsafe_get s (pos+19) = 'r' then (
                                  36
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 'p' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'b' && String.unsafe_get s (pos+17) = 'l' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'd' then (
                          41
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 21 -> (
                  match String.unsafe_get s pos with
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                          match String.unsafe_get s (pos+7) with
                            | 'b' -> (
                                if String.unsafe_get s (pos+8) = 'g' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'l' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = '_' then (
                                  match String.unsafe_get s (pos+16) with
                                    | 'p' -> (
                                        if String.unsafe_get s (pos+17) = 'o' && String.unsafe_get s (pos+18) = 'p' && String.unsafe_get s (pos+19) = 'u' && String.unsafe_get s (pos+20) = 'p' then (
                                          53
                                        )
                                        else (
                                          -1
                                        )
                                      )
                                    | 't' -> (
                                        if String.unsafe_get s (pos+17) = 'h' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'm' && String.unsafe_get s (pos+20) = 'e' then (
                                          54
                                        )
                                        else (
                                          -1
                                        )
                                      )
                                    | _ -> (
                                        -1
                                      )
                                )
                                else (
                                  -1
                                )
                              )
                            | 'f' -> (
                                match String.unsafe_get s (pos+8) with
                                  | 'g' -> (
                                      if String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'l' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'p' && String.unsafe_get s (pos+17) = 'o' && String.unsafe_get s (pos+18) = 'p' && String.unsafe_get s (pos+19) = 'u' && String.unsafe_get s (pos+20) = 'p' then (
                                        68
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | 'o' -> (
                                      if String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'm' && String.unsafe_get s (pos+11) = 'a' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'o' && String.unsafe_get s (pos+15) = 'n' && String.unsafe_get s (pos+16) = '_' && String.unsafe_get s (pos+17) = 's' && String.unsafe_get s (pos+18) = 'a' && String.unsafe_get s (pos+19) = 'v' && String.unsafe_get s (pos+20) = 'e' then (
                                        69
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | _ -> (
                                      -1
                                    )
                              )
                            | 's' -> (
                                if String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'k' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'y' && String.unsafe_get s (pos+16) = 's' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'n' && String.unsafe_get s (pos+20) = 'd' then (
                                  89
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'm' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = '_' then (
                          match String.unsafe_get s (pos+9) with
                            | '1' -> (
                                if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'f' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'l' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 's' && String.unsafe_get s (pos+16) = 'c' && String.unsafe_get s (pos+17) = 'r' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'e' && String.unsafe_get s (pos+20) = 'n' then (
                                  12
                                )
                                else (
                                  -1
                                )
                              )
                            | '2' -> (
                                if String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'f' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'l' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 's' && String.unsafe_get s (pos+16) = 'c' && String.unsafe_get s (pos+17) = 'r' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'e' && String.unsafe_get s (pos+20) = 'n' then (
                                  17
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'o' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'f' && String.unsafe_get s (pos+14) = 'g' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'c' && String.unsafe_get s (pos+17) = 'o' && String.unsafe_get s (pos+18) = 'l' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'r' then (
                          33
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 22 -> (
                  match String.unsafe_get s pos with
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                          match String.unsafe_get s (pos+7) with
                            | 'c' -> (
                                if String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'm' && String.unsafe_get s (pos+10) = 'p' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'f' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'n' && String.unsafe_get s (pos+21) = 't' then (
                                  57
                                )
                                else (
                                  -1
                                )
                              )
                            | 's' -> (
                                if String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'k' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'y' && String.unsafe_get s (pos+16) = 's' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'h' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'm' && String.unsafe_get s (pos+21) = 'e' then (
                                  90
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'o' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' then (
                          match String.unsafe_get s (pos+3) with
                            | 'l' -> (
                                if String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 'l' && String.unsafe_get s (pos+16) = 't' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'r' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'w' && String.unsafe_get s (pos+21) = 's' then (
                                  30
                                )
                                else (
                                  -1
                                )
                              )
                            | 'p' -> (
                                if String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 'u' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'f' && String.unsafe_get s (pos+15) = 'g' && String.unsafe_get s (pos+16) = '_' && String.unsafe_get s (pos+17) = 'c' && String.unsafe_get s (pos+18) = 'o' && String.unsafe_get s (pos+19) = 'l' && String.unsafe_get s (pos+20) = 'o' && String.unsafe_get s (pos+21) = 'r' then (
                                  34
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'd' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'f' && String.unsafe_get s (pos+11) = 'f' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = 'a' && String.unsafe_get s (pos+16) = 'p' && String.unsafe_get s (pos+17) = 'h' && String.unsafe_get s (pos+18) = 'i' && String.unsafe_get s (pos+19) = 'c' && String.unsafe_get s (pos+20) = 'a' && String.unsafe_get s (pos+21) = 'l' then (
                          38
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 24 -> (
                  match String.unsafe_get s pos with
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                          match String.unsafe_get s (pos+7) with
                            | 'i' -> (
                                if String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'd' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'e' && String.unsafe_get s (pos+15) = 'm' && String.unsafe_get s (pos+16) = 'p' && String.unsafe_get s (pos+17) = 't' && String.unsafe_get s (pos+18) = 'y' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'l' && String.unsafe_get s (pos+21) = 'i' && String.unsafe_get s (pos+22) = 'n' && String.unsafe_get s (pos+23) = 'e' then (
                                  72
                                )
                                else (
                                  -1
                                )
                              )
                            | 's' -> (
                                match String.unsafe_get s (pos+8) with
                                  | 'a' -> (
                                      if String.unsafe_get s (pos+9) = 'v' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 'l' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'b' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 'f' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'o' && String.unsafe_get s (pos+22) = 'm' && String.unsafe_get s (pos+23) = 'p' then (
                                        84
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | 'h' -> (
                                      if String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'w' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'l' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = '_' && String.unsafe_get s (pos+17) = 'n' && String.unsafe_get s (pos+18) = 'u' && String.unsafe_get s (pos+19) = 'm' && String.unsafe_get s (pos+20) = 'b' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = 'r' && String.unsafe_get s (pos+23) = 's' then (
                                        87
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | _ -> (
                                      -1
                                    )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'r' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'm' && String.unsafe_get s (pos+5) = 'b' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'w' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = 'd' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'w' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 'o' && String.unsafe_get s (pos+19) = 'm' && String.unsafe_get s (pos+20) = 'e' && String.unsafe_get s (pos+21) = 't' && String.unsafe_get s (pos+22) = 'r' && String.unsafe_get s (pos+23) = 'y' then (
                          40
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 25 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                    match String.unsafe_get s (pos+7) with
                      | 'c' -> (
                          if String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'm' && String.unsafe_get s (pos+10) = 'p' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'o' && String.unsafe_get s (pos+19) = 'p' && String.unsafe_get s (pos+20) = 'a' && String.unsafe_get s (pos+21) = 'c' && String.unsafe_get s (pos+22) = 'i' && String.unsafe_get s (pos+23) = 't' && String.unsafe_get s (pos+24) = 'y' then (
                            60
                          )
                          else (
                            -1
                          )
                        )
                      | 'r' -> (
                          if String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'g' && String.unsafe_get s (pos+10) = 'h' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'm' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 'r' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'i' && String.unsafe_get s (pos+18) = 'n' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'o' && String.unsafe_get s (pos+22) = 'l' && String.unsafe_get s (pos+23) = 'o' && String.unsafe_get s (pos+24) = 'r' then (
                            82
                          )
                          else (
                            -1
                          )
                        )
                      | 's' -> (
                          if String.unsafe_get s (pos+8) = 'h' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'w' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'g' && String.unsafe_get s (pos+13) = 'l' && String.unsafe_get s (pos+14) = 'o' && String.unsafe_get s (pos+15) = 'b' && String.unsafe_get s (pos+16) = 'a' && String.unsafe_get s (pos+17) = 'l' && String.unsafe_get s (pos+18) = '_' && String.unsafe_get s (pos+19) = 'g' && String.unsafe_get s (pos+20) = 'u' && String.unsafe_get s (pos+21) = 't' && String.unsafe_get s (pos+22) = 't' && String.unsafe_get s (pos+23) = 'e' && String.unsafe_get s (pos+24) = 'r' then (
                            86
                          )
                          else (
                            -1
                          )
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 26 -> (
                  match String.unsafe_get s pos with
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'u' && String.unsafe_get s (pos+9) = 'r' then (
                          match String.unsafe_get s (pos+10) with
                            | 'r' -> (
                                if String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'n' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = '_' && String.unsafe_get s (pos+15) = 'l' && String.unsafe_get s (pos+16) = 'i' && String.unsafe_get s (pos+17) = 'n' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'b' && String.unsafe_get s (pos+21) = 'o' && String.unsafe_get s (pos+22) = 'r' && String.unsafe_get s (pos+23) = 'd' && String.unsafe_get s (pos+24) = 'e' && String.unsafe_get s (pos+25) = 'r' then (
                                  61
                                )
                                else (
                                  -1
                                )
                              )
                            | 's' -> (
                                if String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'r' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 's' && String.unsafe_get s (pos+16) = 'p' && String.unsafe_get s (pos+17) = 'e' && String.unsafe_get s (pos+18) = 'c' && String.unsafe_get s (pos+19) = 't' && String.unsafe_get s (pos+20) = '_' && String.unsafe_get s (pos+21) = 'r' && String.unsafe_get s (pos+22) = 'a' && String.unsafe_get s (pos+23) = 't' && String.unsafe_get s (pos+24) = 'i' && String.unsafe_get s (pos+25) = 'o' then (
                                  62
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'm' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'v' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'p' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'f' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 'r' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'f' && String.unsafe_get s (pos+17) = 'u' && String.unsafe_get s (pos+18) = 'l' && String.unsafe_get s (pos+19) = 'l' && String.unsafe_get s (pos+20) = 's' && String.unsafe_get s (pos+21) = 'c' && String.unsafe_get s (pos+22) = 'r' && String.unsafe_get s (pos+23) = 'e' && String.unsafe_get s (pos+24) = 'e' && String.unsafe_get s (pos+25) = 'n' then (
                          18
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 27 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                    match String.unsafe_get s (pos+7) with
                      | 'c' -> (
                          if String.unsafe_get s (pos+8) = 'o' then (
                            match String.unsafe_get s (pos+9) with
                              | 'd' -> (
                                  if String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'f' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'l' && String.unsafe_get s (pos+15) = 'd' && String.unsafe_get s (pos+16) = 'i' && String.unsafe_get s (pos+17) = 'n' && String.unsafe_get s (pos+18) = 'g' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'e' && String.unsafe_get s (pos+21) = 'n' && String.unsafe_get s (pos+22) = 'a' && String.unsafe_get s (pos+23) = 'b' && String.unsafe_get s (pos+24) = 'l' && String.unsafe_get s (pos+25) = 'e' && String.unsafe_get s (pos+26) = 'd' then (
                                    56
                                  )
                                  else (
                                    -1
                                  )
                                )
                              | 'm' -> (
                                  if String.unsafe_get s (pos+10) = 'p' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'd' && String.unsafe_get s (pos+19) = 'e' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'o' && String.unsafe_get s (pos+22) = 'r' && String.unsafe_get s (pos+23) = 'a' && String.unsafe_get s (pos+24) = 't' && String.unsafe_get s (pos+25) = 'e' && String.unsafe_get s (pos+26) = 'd' then (
                                    59
                                  )
                                  else (
                                    -1
                                  )
                                )
                              | _ -> (
                                  -1
                                )
                          )
                          else (
                            -1
                          )
                        )
                      | 'r' -> (
                          if String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'g' && String.unsafe_get s (pos+10) = 'h' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'm' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 'r' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'i' && String.unsafe_get s (pos+18) = 'n' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'v' && String.unsafe_get s (pos+21) = 'i' && String.unsafe_get s (pos+22) = 's' && String.unsafe_get s (pos+23) = 'i' && String.unsafe_get s (pos+24) = 'b' && String.unsafe_get s (pos+25) = 'l' && String.unsafe_get s (pos+26) = 'e' then (
                            83
                          )
                          else (
                            -1
                          )
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 28 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                    match String.unsafe_get s (pos+7) with
                      | 'c' -> (
                          if String.unsafe_get s (pos+8) = 'u' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'm' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 't' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 'm' && String.unsafe_get s (pos+17) = 'p' && String.unsafe_get s (pos+18) = 'l' && String.unsafe_get s (pos+19) = '_' && String.unsafe_get s (pos+20) = 'f' && String.unsafe_get s (pos+21) = 'i' && String.unsafe_get s (pos+22) = 'l' && String.unsafe_get s (pos+23) = 'e' && String.unsafe_get s (pos+24) = 'n' && String.unsafe_get s (pos+25) = 'a' && String.unsafe_get s (pos+26) = 'm' && String.unsafe_get s (pos+27) = 'e' then (
                            63
                          )
                          else (
                            -1
                          )
                        )
                      | 's' -> (
                          match String.unsafe_get s (pos+8) with
                            | 'e' -> (
                                if String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'c' && String.unsafe_get s (pos+12) = 'h' && String.unsafe_get s (pos+13) = '_' && String.unsafe_get s (pos+14) = 'w' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'r' && String.unsafe_get s (pos+17) = 'd' && String.unsafe_get s (pos+18) = '_' && String.unsafe_get s (pos+19) = 'a' && String.unsafe_get s (pos+20) = 't' && String.unsafe_get s (pos+21) = '_' && String.unsafe_get s (pos+22) = 'c' && String.unsafe_get s (pos+23) = 'u' && String.unsafe_get s (pos+24) = 'r' && String.unsafe_get s (pos+25) = 's' && String.unsafe_get s (pos+26) = 'o' && String.unsafe_get s (pos+27) = 'r' then (
                                  85
                                )
                                else (
                                  -1
                                )
                              )
                            | 'h' -> (
                                if String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'w' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'w' && String.unsafe_get s (pos+13) = 'h' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 't' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = 's' && String.unsafe_get s (pos+18) = 'p' && String.unsafe_get s (pos+19) = 'a' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = '_' && String.unsafe_get s (pos+23) = 'c' && String.unsafe_get s (pos+24) = 'h' && String.unsafe_get s (pos+25) = 'a' && String.unsafe_get s (pos+26) = 'r' && String.unsafe_get s (pos+27) = 's' then (
                                  88
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 29 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'h' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'g' && String.unsafe_get s (pos+10) = 'h' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'g' && String.unsafe_get s (pos+14) = 'h' && String.unsafe_get s (pos+15) = 't' && String.unsafe_get s (pos+16) = '_' && String.unsafe_get s (pos+17) = 'c' && String.unsafe_get s (pos+18) = 'u' && String.unsafe_get s (pos+19) = 'r' && String.unsafe_get s (pos+20) = 'r' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = 'n' && String.unsafe_get s (pos+23) = 't' && String.unsafe_get s (pos+24) = '_' && String.unsafe_get s (pos+25) = 'l' && String.unsafe_get s (pos+26) = 'i' && String.unsafe_get s (pos+27) = 'n' && String.unsafe_get s (pos+28) = 'e' then (
                    70
                  )
                  else (
                    -1
                  )
                )
              | 31 -> (
                  match String.unsafe_get s pos with
                    | 'd' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 'a' && String.unsafe_get s (pos+12) = 'g' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = '_' && String.unsafe_get s (pos+15) = 'p' && String.unsafe_get s (pos+16) = 'a' && String.unsafe_get s (pos+17) = 'n' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 's' && String.unsafe_get s (pos+20) = '_' && String.unsafe_get s (pos+21) = 's' && String.unsafe_get s (pos+22) = 'e' && String.unsafe_get s (pos+23) = 'p' && String.unsafe_get s (pos+24) = 'a' && String.unsafe_get s (pos+25) = 'r' && String.unsafe_get s (pos+26) = 'a' && String.unsafe_get s (pos+27) = 't' && String.unsafe_get s (pos+28) = 'e' && String.unsafe_get s (pos+29) = 'l' && String.unsafe_get s (pos+30) = 'y' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                          match String.unsafe_get s (pos+7) with
                            | 'a' -> (
                                if String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'y' && String.unsafe_get s (pos+15) = 'p' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 't' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'o' && String.unsafe_get s (pos+21) = 'l' && String.unsafe_get s (pos+22) = 't' && String.unsafe_get s (pos+23) = 'i' && String.unsafe_get s (pos+24) = 'p' && String.unsafe_get s (pos+25) = 's' && String.unsafe_get s (pos+26) = '_' && String.unsafe_get s (pos+27) = 'i' && String.unsafe_get s (pos+28) = 'm' && String.unsafe_get s (pos+29) = 'p' && String.unsafe_get s (pos+30) = 'l' then (
                                  50
                                )
                                else (
                                  -1
                                )
                              )
                            | 'c' -> (
                                if String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'm' && String.unsafe_get s (pos+10) = 'p' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'e' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'i' && String.unsafe_get s (pos+15) = 'o' && String.unsafe_get s (pos+16) = 'n' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 'g' && String.unsafe_get s (pos+19) = 'r' && String.unsafe_get s (pos+20) = 'e' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = 'k' && String.unsafe_get s (pos+23) = '_' && String.unsafe_get s (pos+24) = 'l' && String.unsafe_get s (pos+25) = 'e' && String.unsafe_get s (pos+26) = 't' && String.unsafe_get s (pos+27) = 't' && String.unsafe_get s (pos+28) = 'e' && String.unsafe_get s (pos+29) = 'r' && String.unsafe_get s (pos+30) = 's' then (
                                  58
                                )
                                else (
                                  -1
                                )
                              )
                            | 'm' -> (
                                if String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'k' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'c' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'u' && String.unsafe_get s (pos+16) = 'r' && String.unsafe_get s (pos+17) = 'r' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'n' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = 's' && String.unsafe_get s (pos+23) = '_' && String.unsafe_get s (pos+24) = 'e' && String.unsafe_get s (pos+25) = 'n' && String.unsafe_get s (pos+26) = 'a' && String.unsafe_get s (pos+27) = 'b' && String.unsafe_get s (pos+28) = 'l' && String.unsafe_get s (pos+29) = 'e' && String.unsafe_get s (pos+30) = 'd' then (
                                  75
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 32 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' then (
                    match String.unsafe_get s (pos+7) with
                      | 'a' -> (
                          if String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'y' && String.unsafe_get s (pos+15) = 'p' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 't' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'o' && String.unsafe_get s (pos+21) = 'l' && String.unsafe_get s (pos+22) = 't' && String.unsafe_get s (pos+23) = 'i' && String.unsafe_get s (pos+24) = 'p' && String.unsafe_get s (pos+25) = 's' && String.unsafe_get s (pos+26) = '_' && String.unsafe_get s (pos+27) = 'd' && String.unsafe_get s (pos+28) = 'e' && String.unsafe_get s (pos+29) = 'l' && String.unsafe_get s (pos+30) = 'a' && String.unsafe_get s (pos+31) = 'y' then (
                            49
                          )
                          else (
                            -1
                          )
                        )
                      | 'm' -> (
                          if String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'k' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'c' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'u' && String.unsafe_get s (pos+16) = 'r' && String.unsafe_get s (pos+17) = 'r' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'n' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = 's' && String.unsafe_get s (pos+23) = '_' && String.unsafe_get s (pos+24) = 'b' && String.unsafe_get s (pos+25) = 'g' && String.unsafe_get s (pos+26) = '_' && String.unsafe_get s (pos+27) = 'c' && String.unsafe_get s (pos+28) = 'o' && String.unsafe_get s (pos+29) = 'l' && String.unsafe_get s (pos+30) = 'o' && String.unsafe_get s (pos+31) = 'r' then (
                            77
                          )
                          else (
                            -1
                          )
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 34 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'y' && String.unsafe_get s (pos+15) = 'p' && String.unsafe_get s (pos+16) = 'e' && String.unsafe_get s (pos+17) = '_' && String.unsafe_get s (pos+18) = 't' && String.unsafe_get s (pos+19) = 'o' && String.unsafe_get s (pos+20) = 'o' && String.unsafe_get s (pos+21) = 'l' && String.unsafe_get s (pos+22) = 't' && String.unsafe_get s (pos+23) = 'i' && String.unsafe_get s (pos+24) = 'p' && String.unsafe_get s (pos+25) = 's' && String.unsafe_get s (pos+26) = '_' && String.unsafe_get s (pos+27) = 'e' && String.unsafe_get s (pos+28) = 'n' && String.unsafe_get s (pos+29) = 'a' && String.unsafe_get s (pos+30) = 'b' && String.unsafe_get s (pos+31) = 'l' && String.unsafe_get s (pos+32) = 'e' && String.unsafe_get s (pos+33) = 'd' then (
                    48
                  )
                  else (
                    -1
                  )
                )
              | 35 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'm' && String.unsafe_get s (pos+11) = 'l' && String.unsafe_get s (pos+12) = 'd' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = '_' && String.unsafe_get s (pos+16) = 'p' && String.unsafe_get s (pos+17) = 'a' && String.unsafe_get s (pos+18) = 'r' && String.unsafe_get s (pos+19) = 'a' && String.unsafe_get s (pos+20) = 'g' && String.unsafe_get s (pos+21) = 'r' && String.unsafe_get s (pos+22) = 'a' && String.unsafe_get s (pos+23) = 'p' && String.unsafe_get s (pos+24) = 'h' && String.unsafe_get s (pos+25) = '_' && String.unsafe_get s (pos+26) = 'b' && String.unsafe_get s (pos+27) = 'g' && String.unsafe_get s (pos+28) = 'c' && String.unsafe_get s (pos+29) = 'o' && String.unsafe_get s (pos+30) = 'l' && String.unsafe_get s (pos+31) = 'o' && String.unsafe_get s (pos+32) = 'r' && String.unsafe_get s (pos+33) = '_' then (
                    match String.unsafe_get s (pos+34) with
                      | '1' -> (
                          78
                        )
                      | '2' -> (
                          79
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 36 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'k' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'o' && String.unsafe_get s (pos+13) = 'c' && String.unsafe_get s (pos+14) = 'c' && String.unsafe_get s (pos+15) = 'u' && String.unsafe_get s (pos+16) = 'r' && String.unsafe_get s (pos+17) = 'r' && String.unsafe_get s (pos+18) = 'e' && String.unsafe_get s (pos+19) = 'n' && String.unsafe_get s (pos+20) = 'c' && String.unsafe_get s (pos+21) = 'e' && String.unsafe_get s (pos+22) = 's' && String.unsafe_get s (pos+23) = '_' && String.unsafe_get s (pos+24) = 'u' && String.unsafe_get s (pos+25) = 'n' && String.unsafe_get s (pos+26) = 'd' && String.unsafe_get s (pos+27) = 'e' && String.unsafe_get s (pos+28) = 'r' && String.unsafe_get s (pos+29) = '_' && String.unsafe_get s (pos+30) = 'c' && String.unsafe_get s (pos+31) = 'u' && String.unsafe_get s (pos+32) = 'r' && String.unsafe_get s (pos+33) = 's' && String.unsafe_get s (pos+34) = 'o' && String.unsafe_get s (pos+35) = 'r' then (
                    76
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_timestamp := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_build_parallel := (
                  (
                    read__int_option
                  ) p lb
                );
              )
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_build_verbosity := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_check_updates := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 4 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_detach_message_panes_separately := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 5 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_font := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 6 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_geometry_delayed := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 7 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_hmessages_width := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 8 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_max_view_1_menubar := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 9 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_max_view_1_toolbar := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 10 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_max_view_1_tabbar := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 11 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_max_view_1_messages := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 12 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_max_view_1_fullscreen := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 13 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_max_view_2_menubar := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 14 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_max_view_2_toolbar := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 15 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_max_view_2_tabbar := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 16 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_max_view_2_messages := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 17 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_max_view_2_fullscreen := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 18 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_max_view_prefer_fullscreen := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 19 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_menubar_buttons := (
                  (
                    read__int_list
                  ) p lb
                );
              )
            | 20 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_odoc_font := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 21 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_outline_show_types := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 22 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_outline_width := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 23 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_outline_color_types := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 24 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_outline_color_nor_bg := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 25 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_outline_color_nor_fg := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 26 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_outline_color_sel_bg := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 27 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_outline_color_sel_fg := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 28 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_outline_color_act_bg := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 29 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_outline_color_act_fg := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 30 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_outline_color_alt_rows := (
                  (
                    read__float_option
                  ) p lb
                );
              )
            | 31 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_output_font := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 32 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_output_bg_color := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 33 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_output_stdin_fg_color := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 34 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_output_stdout_fg_color := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 35 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_output_err_fg_color := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 36 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_output_warn_fg_color := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 37 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_program_diff := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 38 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_program_diff_graphical := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 39 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_program_pdf_viewer := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 40 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_remember_window_geometry := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 41 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_splashscreen_enabled := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 42 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_tab_label_type := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 43 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_tab_pos := (
                  (
                    fun p lb ->
                      Yojson.Safe.read_space p lb;
                      match Yojson.Safe.start_any_variant p lb with
                        | `Edgy_bracket -> (
                            match Yojson.Safe.read_ident p lb with
                              | "TOP" ->
                                Yojson.Safe.read_space p lb;
                                Yojson.Safe.read_gt p lb;
                                `TOP
                              | "RIGHT" ->
                                Yojson.Safe.read_space p lb;
                                Yojson.Safe.read_gt p lb;
                                `RIGHT
                              | "BOTTOM" ->
                                Yojson.Safe.read_space p lb;
                                Yojson.Safe.read_gt p lb;
                                `BOTTOM
                              | "LEFT" ->
                                Yojson.Safe.read_space p lb;
                                Yojson.Safe.read_gt p lb;
                                `LEFT
                              | x ->
                                Atdgen_runtime.Oj_run.invalid_variant_tag p x
                          )
                        | `Double_quote -> (
                            match Yojson.Safe.finish_string p lb with
                              | "TOP" ->
                                `TOP
                              | "RIGHT" ->
                                `RIGHT
                              | "BOTTOM" ->
                                `BOTTOM
                              | "LEFT" ->
                                `LEFT
                              | x ->
                                Atdgen_runtime.Oj_run.invalid_variant_tag p x
                          )
                        | `Square_bracket -> (
                            match Atdgen_runtime.Oj_run.read_string p lb with
                              | x ->
                                Atdgen_runtime.Oj_run.invalid_variant_tag p x
                          )
                  ) p lb
                );
              )
            | 44 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_tab_vertical_text := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 45 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_theme := (
                  (
                    read__string_option
                  ) p lb
                );
              )
            | 46 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_theme_is_dark := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 47 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_vmessages_height := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 48 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_annot_type_tooltips_enabled := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 49 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_annot_type_tooltips_delay := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 50 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_annot_type_tooltips_impl := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 51 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_bak := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 52 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_base_font := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 53 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_bg_color_popup := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 54 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_bg_color_theme := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 55 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_bg_color_user := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 56 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_code_folding_enabled := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 57 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_completion_font := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 58 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_completion_greek_letters := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 59 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_completion_decorated := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 60 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_completion_opacity := (
                  (
                    read__float_option
                  ) p lb
                );
              )
            | 61 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_current_line_border := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 62 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_cursor_aspect_ratio := (
                  (
                    Atdgen_runtime.Oj_run.read_number
                  ) p lb
                );
              )
            | 63 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_custom_templ_filename := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 64 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_dot_leaders := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 65 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_err_gutter := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 66 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_err_tooltip := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 67 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_err_underline := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 68 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_fg_color_popup := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 69 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_format_on_save := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 70 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_highlight_current_line := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 71 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_indent_config := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 72 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_indent_empty_line := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 73 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_indent_lines := (
                  (
                    fun p lb ->
                      Yojson.Safe.read_space p lb;
                      let std_tuple = Yojson.Safe.start_any_tuple p lb in
                      let len = ref 0 in
                      let end_of_tuple = ref false in
                      (try
                        let x0 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_bool
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x1 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_string
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x2 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_string
                            ) p lb
                          in
                          incr len;
                          (try
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          with Yojson.End_of_tuple -> end_of_tuple := true);
                          x
                        in
                        if not !end_of_tuple then (
                          try
                            while true do
                              Yojson.Safe.skip_json p lb;
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                            done
                          with Yojson.End_of_tuple -> ()
                        );
                        (x0, x1, x2)
                      with Yojson.End_of_tuple ->
                        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1; 2 ]);
                  ) p lb
                );
              )
            | 74 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_left_margin := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 75 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_mark_occurrences_enabled := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 76 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_mark_occurrences_under_cursor := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 77 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_mark_occurrences_bg_color := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 78 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_ocamldoc_paragraph_bgcolor_1 := (
                  (
                    read__string_option
                  ) p lb
                );
              )
            | 79 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_ocamldoc_paragraph_bgcolor_2 := (
                  (
                    read__string_option
                  ) p lb
                );
              )
            | 80 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_pixels_lines := (
                  (
                    fun p lb ->
                      Yojson.Safe.read_space p lb;
                      let std_tuple = Yojson.Safe.start_any_tuple p lb in
                      let len = ref 0 in
                      let end_of_tuple = ref false in
                      (try
                        let x0 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int
                            ) p lb
                          in
                          incr len;
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          x
                        in
                        let x1 =
                          let x =
                            (
                              Atdgen_runtime.Oj_run.read_int
                            ) p lb
                          in
                          incr len;
                          (try
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          with Yojson.End_of_tuple -> end_of_tuple := true);
                          x
                        in
                        if not !end_of_tuple then (
                          try
                            while true do
                              Yojson.Safe.skip_json p lb;
                              Yojson.Safe.read_space p lb;
                              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                            done
                          with Yojson.End_of_tuple -> ()
                        );
                        (x0, x1)
                      with Yojson.End_of_tuple ->
                        Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                  ) p lb
                );
              )
            | 81 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_right_margin := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 82 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_right_margin_color := (
                  (
                    Atdgen_runtime.Oj_run.read_string
                  ) p lb
                );
              )
            | 83 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_right_margin_visible := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 84 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_save_all_bef_comp := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 85 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_search_word_at_cursor := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 86 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_show_global_gutter := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 87 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_show_line_numbers := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 88 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_show_whitespace_chars := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 89 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_smart_keys_end := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 90 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_smart_keys_home := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 91 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_tab_spaces := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 92 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_tab_width := (
                  (
                    Atdgen_runtime.Oj_run.read_int
                  ) p lb
                );
              )
            | 93 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_tags := (
                  (
                    read__editor_tag_list
                  ) p lb
                );
              )
            | 94 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_tags_dark := (
                  (
                    read__editor_tag_list
                  ) p lb
                );
              )
            | 95 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_trim_lines := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | 96 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_editor_wrap := (
                  (
                    Atdgen_runtime.Oj_run.read_bool
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            timestamp = !field_timestamp;
            build_parallel = !field_build_parallel;
            build_verbosity = !field_build_verbosity;
            check_updates = !field_check_updates;
            detach_message_panes_separately = !field_detach_message_panes_separately;
            font = !field_font;
            geometry_delayed = !field_geometry_delayed;
            hmessages_width = !field_hmessages_width;
            max_view_1_menubar = !field_max_view_1_menubar;
            max_view_1_toolbar = !field_max_view_1_toolbar;
            max_view_1_tabbar = !field_max_view_1_tabbar;
            max_view_1_messages = !field_max_view_1_messages;
            max_view_1_fullscreen = !field_max_view_1_fullscreen;
            max_view_2_menubar = !field_max_view_2_menubar;
            max_view_2_toolbar = !field_max_view_2_toolbar;
            max_view_2_tabbar = !field_max_view_2_tabbar;
            max_view_2_messages = !field_max_view_2_messages;
            max_view_2_fullscreen = !field_max_view_2_fullscreen;
            max_view_prefer_fullscreen = !field_max_view_prefer_fullscreen;
            menubar_buttons = !field_menubar_buttons;
            odoc_font = !field_odoc_font;
            outline_show_types = !field_outline_show_types;
            outline_width = !field_outline_width;
            outline_color_types = !field_outline_color_types;
            outline_color_nor_bg = !field_outline_color_nor_bg;
            outline_color_nor_fg = !field_outline_color_nor_fg;
            outline_color_sel_bg = !field_outline_color_sel_bg;
            outline_color_sel_fg = !field_outline_color_sel_fg;
            outline_color_act_bg = !field_outline_color_act_bg;
            outline_color_act_fg = !field_outline_color_act_fg;
            outline_color_alt_rows = !field_outline_color_alt_rows;
            output_font = !field_output_font;
            output_bg_color = !field_output_bg_color;
            output_stdin_fg_color = !field_output_stdin_fg_color;
            output_stdout_fg_color = !field_output_stdout_fg_color;
            output_err_fg_color = !field_output_err_fg_color;
            output_warn_fg_color = !field_output_warn_fg_color;
            program_diff = !field_program_diff;
            program_diff_graphical = !field_program_diff_graphical;
            program_pdf_viewer = !field_program_pdf_viewer;
            remember_window_geometry = !field_remember_window_geometry;
            splashscreen_enabled = !field_splashscreen_enabled;
            tab_label_type = !field_tab_label_type;
            tab_pos = !field_tab_pos;
            tab_vertical_text = !field_tab_vertical_text;
            theme = !field_theme;
            theme_is_dark = !field_theme_is_dark;
            vmessages_height = !field_vmessages_height;
            editor_annot_type_tooltips_enabled = !field_editor_annot_type_tooltips_enabled;
            editor_annot_type_tooltips_delay = !field_editor_annot_type_tooltips_delay;
            editor_annot_type_tooltips_impl = !field_editor_annot_type_tooltips_impl;
            editor_bak = !field_editor_bak;
            editor_base_font = !field_editor_base_font;
            editor_bg_color_popup = !field_editor_bg_color_popup;
            editor_bg_color_theme = !field_editor_bg_color_theme;
            editor_bg_color_user = !field_editor_bg_color_user;
            editor_code_folding_enabled = !field_editor_code_folding_enabled;
            editor_completion_font = !field_editor_completion_font;
            editor_completion_greek_letters = !field_editor_completion_greek_letters;
            editor_completion_decorated = !field_editor_completion_decorated;
            editor_completion_opacity = !field_editor_completion_opacity;
            editor_current_line_border = !field_editor_current_line_border;
            editor_cursor_aspect_ratio = !field_editor_cursor_aspect_ratio;
            editor_custom_templ_filename = !field_editor_custom_templ_filename;
            editor_dot_leaders = !field_editor_dot_leaders;
            editor_err_gutter = !field_editor_err_gutter;
            editor_err_tooltip = !field_editor_err_tooltip;
            editor_err_underline = !field_editor_err_underline;
            editor_fg_color_popup = !field_editor_fg_color_popup;
            editor_format_on_save = !field_editor_format_on_save;
            editor_highlight_current_line = !field_editor_highlight_current_line;
            editor_indent_config = !field_editor_indent_config;
            editor_indent_empty_line = !field_editor_indent_empty_line;
            editor_indent_lines = !field_editor_indent_lines;
            editor_left_margin = !field_editor_left_margin;
            editor_mark_occurrences_enabled = !field_editor_mark_occurrences_enabled;
            editor_mark_occurrences_under_cursor = !field_editor_mark_occurrences_under_cursor;
            editor_mark_occurrences_bg_color = !field_editor_mark_occurrences_bg_color;
            editor_ocamldoc_paragraph_bgcolor_1 = !field_editor_ocamldoc_paragraph_bgcolor_1;
            editor_ocamldoc_paragraph_bgcolor_2 = !field_editor_ocamldoc_paragraph_bgcolor_2;
            editor_pixels_lines = !field_editor_pixels_lines;
            editor_right_margin = !field_editor_right_margin;
            editor_right_margin_color = !field_editor_right_margin_color;
            editor_right_margin_visible = !field_editor_right_margin_visible;
            editor_save_all_bef_comp = !field_editor_save_all_bef_comp;
            editor_search_word_at_cursor = !field_editor_search_word_at_cursor;
            editor_show_global_gutter = !field_editor_show_global_gutter;
            editor_show_line_numbers = !field_editor_show_line_numbers;
            editor_show_whitespace_chars = !field_editor_show_whitespace_chars;
            editor_smart_keys_end = !field_editor_smart_keys_end;
            editor_smart_keys_home = !field_editor_smart_keys_home;
            editor_tab_spaces = !field_editor_tab_spaces;
            editor_tab_width = !field_editor_tab_width;
            editor_tags = !field_editor_tags;
            editor_tags_dark = !field_editor_tags_dark;
            editor_trim_lines = !field_editor_trim_lines;
            editor_wrap = !field_editor_wrap;
          }
         : settings)
      )
)
let settings_of_string s =
  read_settings (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
