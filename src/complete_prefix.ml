open Merlin
open GUtil
module ColorOps = Color
open Preferences
open Printf

module String_utils = struct
  let rec locate_intersection left right =
    let len_right = String.length right in
    if len_right = 0 then String.length left, 0
    else
      let re = Str.regexp (Printf.sprintf "%s$" (Str.quote right)) in
      try
        Str.search_backward re left (String.length left),
        len_right
      with Not_found ->
        locate_intersection left (Str.first_chars right (len_right - 1))
end

let re_indent = Str.regexp "^[ ]+"
let re_multi_space = Str.regexp " [ ]+"
let re_newlines = Str.regexp "[\n\r]+"

let color_of_kind = function
  | "Value" -> Preferences.editor_tag_color "structure"
  | "Type" -> Preferences.editor_tag_color "lident"
  | "Module" -> Preferences.editor_tag_color "uident"
  | "Constructor" -> Preferences.editor_tag_color "lident"
  | "Label" -> Preferences.editor_tag_color "label"
  | "Class" -> Preferences.editor_tag_color "lident"
  | "Method" -> Preferences.editor_tag_color "lident"
  | "Signature" -> Preferences.editor_tag_color "lident"
  | "Exn" -> `NAME "red" |> GDraw.color
  | "#" -> Preferences.editor_tag_color "lident"
  | x -> Preferences.editor_tag_color "lident"

let icon_of_kind kind =
  let color = kind |> color_of_kind |> ColorOps.name_of_gdk in
  match kind with
  | "Value" -> sprintf "<span color='%s'>\u{ea8c} </span>" color
  | "Type" -> sprintf "<span color='%s'>\u{1d6bb} </span>" color
  | "Module" -> sprintf "<span color='%s'>\u{f1b2} </span>" color
  | "Constructor" -> sprintf "<span color='%s'>\u{ea88} </span>" color
  | "Label" -> sprintf "<span color='%s'>\u{f0316} </span>" color
  | "Class" -> sprintf "<span color='%s'>\u{eb5b} </span>" color
  | "Method" -> sprintf "<span color='%s'>\u{eb65} </span>" color
  | "Signature" -> sprintf "<span color='%s'>\u{eb61} </span>" color
  | "Exn" -> sprintf "<span color='%s'>\u{f12a} </span>" color
  | "#" -> sprintf "<span color='%s'>\u{f0ad} </span>" color
  | x -> sprintf "<span color='%s'>%s</span>" color x

class virtual completion =
  object
    method virtual complete : GWindow.window -> unit
    method virtual coerce : GObj.widget
    method virtual destroy : unit -> unit
  end

let single_instance : completion option ref = ref None

let create ~compl ~x ~y ~project ~page =
  begin
    match !single_instance with
    | Some instance -> instance#destroy();
    | _ -> ()
  end;
  let window = Gtk_util.window_tooltip compl#coerce ~parent:page ~x ~y () in
  window#misc#hide();
  compl#complete window;
  single_instance := Some compl

class widget ~project ~(page : Editor_page.page) ~x ~y ?packing () =
  let markup_types_color = Lexical_markup.parse Preferences.preferences#get in
  let markup_types_bw = Print_type.markup2 in
  let vbox = GPack.vbox ~spacing:5 ~border_width:0 ?packing () in
  let cols = new GTree.column_list in
  let col_is_exp = cols#add Gobject.Data.boolean in
  let col_kind = cols#add Gobject.Data.string in
  let col_name = cols#add Gobject.Data.string in
  let col_desc = cols#add Gobject.Data.string in
  let col_info = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let renderer = GTree.cell_renderer_text [`SCALE `SMALL; `YPAD 0] in
  let vc_kind = GTree.view_column ~renderer:(renderer, ["markup", col_kind]) () in
  let vc_name = GTree.view_column ~renderer:(renderer, ["text", col_name]) () in
  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let lview = GTree.view ~model ~headers_visible:false ~reorderable:false ~height:200 ~packing:sw#add () in
  let _ = lview#set_enable_search false in
  let _ = lview#set_search_column 1 in
  let _ = lview#append_column vc_kind in
  let _ = lview#append_column vc_name in
  let _ = lview#selection#set_mode `SINGLE in
  let view = page#ocaml_view in
  let buffer = view#buffer in
  let merlin func =
    let filename = match (view#obuffer#as_text_buffer :> Text.buffer)#file with Some file -> file#filename | _ -> "" in
    let source_code = buffer#get_text () in
    func ~filename ~source_code
  in
  let font_name = Preferences.preferences#get.editor_base_font in
  let font_family_monospace =
    String.sub font_name 0 (Option.value (String.rindex_opt font_name ' ') ~default:(String.length font_name)) in
  let code_color = ?? (preferences#get.editor_fg_color_popup) in
  let pending_newline = ref false in
  object (self)
    inherit GObj.widget vbox#as_widget
    inherit completion

    val mutable current_prefix = ""
    val mutable current_prefix_offset_start = 0
    val mutable count = 0
    val first_entry_available = new first_entry_available()
    val loading_complete = new loading_complete()
    val mutable is_destroyed = false
    val mutable page_signals = []
    val mutable buffer_signals = []
    val mutable view_signals = []
    val mutable current_window_info = []

    method complete (window : GWindow.window) =
      let position = buffer#get_iter_at_mark `INSERT in
      let word_start, word_end = page#buffer#as_text_buffer#select_word ~pat:Ocaml_word_bound.longid ~select:false ~search:false () in
      let is_sharp uc = Glib.Utf8.from_unichar uc = "#" in
      let start = position#backward_find_char ~limit:word_start is_sharp in
      let is_method_compl = is_sharp start#char in
      let current_prefix_start = if is_method_compl then start#forward_char else start in
      let prefix = page#buffer#get_text ~start:current_prefix_start ~stop:position () in
      current_prefix_offset_start <- current_prefix_start#offset;
      current_prefix <- prefix;
      count <- 0;
      self#invoke_merlin ~prefix ~position ~expand:(not is_method_compl) window;
      self#connect_signals word_end;
      self#misc#connect#destroy ~callback:begin fun () ->
        self#disconnect_signals();
        window#destroy();
      end |> ignore;
      window#move ~x ~y

    method private connect_signals word_end =
      page_signals <- [
        page#connect#scroll_changed ~callback:(fun _ -> self#destroy());
      ];
      buffer_signals <- [
        view#buffer#connect#mark_set ~callback:begin fun it mark ->
          match GtkText.Mark.get_name mark with
          | Some "insert" ->
              let ins = buffer#get_iter `INSERT in
              if
                ins#offset < current_prefix_offset_start ||
                ins#compare word_end > 0
              then self#destroy();
          | _ -> ()
        end;
        view#buffer#connect#changed ~callback:begin fun () ->
          let x, y = view#get_location_at_cursor () in
          let compl = new widget ~project ~page ~x ~y () in
          create ~compl:(compl :> completion) ~x ~y ~project ~page;
        end;
      ];
      view_signals <-
        [
          view#event#connect#key_press ~callback:begin fun ev ->
            let keyval = GdkEvent.Key.keyval ev in
            if keyval = GdkKeysyms._Escape then begin
              self#destroy();
              true
            end else if keyval = GdkKeysyms._Up then begin
              self#select_row `PREV;
              true
            end else if keyval = GdkKeysyms._Down then begin
              self#select_row `NEXT;
              true
            end else if keyval = GdkKeysyms._Return then begin
              self#selected_path
              |> Option.iter (fun path -> Gmisclib.Idle.add (fun () -> self#apply path));
              true
            end else false
          end;
          view#event#connect#focus_out ~callback:(fun _ -> self#destroy(); false);
          view#event#connect#scroll ~callback:(fun _ -> self#destroy(); false);
        ]

    method private disconnect_signals () =
      page_signals |> List.iter page#disconnect;
      page_signals <- [];
      buffer_signals |> List.iter (GtkSignal.disconnect buffer#as_buffer);
      buffer_signals <- [];
      view_signals |> List.iter (GtkSignal.disconnect view#as_view);
      view_signals <- [];

    method private invoke_merlin ~prefix ~position ?(expand=true) window =
      let position = position#line + 1, position#line_offset in
      merlin@@complete_prefix ~position ~prefix begin fun complete_prefix ->
        window#show();
        begin
          match complete_prefix.entries with
          | first :: others ->
              [first] |> self#add_entries false;
              first_entry_available#call();
              others |> self#add_entries false;
          | [] -> ()
        end;
        if count = 0 || expand then begin
          merlin@@expand_prefix ~position ~prefix begin fun expand_prefix ->
            expand_prefix.entries |> self#add_entries true;
            loading_complete#call count;
          end
        end else
          loading_complete#call count;
      end;

    method private apply path =
      let row = model#get_iter path in
      let name = model#get ~row ~column:col_name in
      let is_expand = model#get ~row ~column:col_is_exp in
      page#view#tbuffer#undo#begin_block ~name:"compl";
      let _, stop = page#buffer#as_text_buffer#select_word ~pat:Ocaml_word_bound.regexp ~select:false ~search:false () in
      self#disconnect_signals();
      if is_expand then begin
        let start = buffer#get_iter_at_char current_prefix_offset_start in
        buffer#delete_interactive ~start ~stop () |> ignore;
        buffer#insert_interactive name |> ignore;
      end else begin
        let a, b = String_utils.locate_intersection current_prefix name in
        let substitute = Str.string_after name b in
        Printf.printf "name=%S current_prefix=%S %d %d %S\n%!" name current_prefix a b substitute;
        let start = buffer#get_iter `INSERT in
        let stop = if start#compare stop >= 0 then start else stop in
        buffer#delete_interactive ~start ~stop () |> ignore;
        buffer#insert_interactive substitute |> ignore;
      end;
      page#view#tbuffer#undo#end_block();
      self#destroy();

    method private select_row direction =
      try
        let path =
          match lview#selection#get_selected_rows with
          | [] -> GTree.Path.create [0]
          | [ path ] when direction = `NEXT -> GTree.Path.next path; path
          | [ path ] when direction = `PREV -> if GTree.Path.prev path then path else GTree.Path.create [0]
          | _ -> assert false
        in
        lview#selection#select_path path;
        lview#scroll_to_cell path vc_kind
      with Gpointer.Null -> ()

    method private markup_of_info_element element =
      let open Odoc_info in
      let markup_of_elements text = text |> List.map self#markup_of_info_element |> String.concat "" in
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
            |> sprintf "<span color='%s' face='%s'>%s</span>" code_color font_family_monospace
        | CodePre code ->
            code
            |> Glib.Markup.escape_text
            |> sprintf "\n<span color='%s' face='%s'>%s</span>\n" code_color font_family_monospace
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
            |> List.map (fun t -> t |> List.map self#markup_of_info_element |> String.concat "")
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
            sprintf "<span color='%s' face='%s'>%s</span>%s" code_color font_family_monospace name
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

    method private markup_of_info info =
      let open Odoc_info in
      let odoc = info_of_string info in
      match odoc.i_desc with
      | None -> ""
      | Some idesc ->
          idesc
          |> List.map self#markup_of_info_element
          |> String.concat ""

    method private show_info () =
      match lview#selection#get_selected_rows with
      | [] -> ()
      | path :: _ ->
          let row = model#get_iter path in
          let desc = model#get ~row ~column:col_desc in
          let info = model#get ~row ~column:col_info in
          let info = String.trim info in
          if String.trim desc <> "" || String.trim info <> "" then begin
            Gmisclib.Idle.add begin fun () ->
              let markup =
                Printf.sprintf "<span size='small' font_family='%s'>%s</span>%s<span size='small'>%s</span>"
                  font_family_monospace (markup_types_bw desc)
                  (if info <> "" then "\n\n" else "")
                  (self#markup_of_info info)
              in
              self#display_window_info path markup
            end
          end

    method private display_window_info path markup =
      let create_window ~x ~y ?width ?height ?show child =
        current_window_info |> List.iter (fun w -> w#destroy());
        let window = Gtk_util.window_tooltip child ~parent:page ~x ~y ?width ?height ?show () in
        current_window_info <- window :: current_window_info;
        self#misc#connect#destroy ~callback:window#destroy |> ignore;
        window
      in
      let row_area = lview#get_cell_area ~path () in
      let r0 = self#misc#allocation in
      let wx, wy = Gdk.Window.get_position self#misc#toplevel#misc#window in
      let x0 = wx + r0.Gtk.width in
      let y0 = wy + Gdk.Rectangle.y row_area in
      let label_info = GMisc.label ~markup ~xalign:0.0 ~yalign:0.0 ~xpad:10 ~ypad:10 ~line_wrap:true () in
      let window_info = create_window ~x:x0 ~y:y0 ~show:false label_info#coerce in
      window_info#resize ~width:1 ~height:1;
      Gmisclib.Idle.add ~prio:100 begin fun () ->
        window_info#show();
        let _ = Gtk_util.move_window_within_screen_bounds window_info x0 y0 in
        let r = window_info#misc#allocation in
        if r.height > Gdk.Screen.height() then begin
          let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC () in
          let vp = GBin.viewport ~packing:sw#add () in
          label_info#misc#reparent vp#coerce;
          let width = r.width + 21 in
          let height = Gdk.Screen.height() - y0 - 13 in
          create_window ~x:x0 ~y:y0 ~width ~height sw#coerce |> ignore;
        end
      end

    method private selected_path =
      try
        match lview#selection#get_selected_rows with
        | [path] -> Some path
        | _ -> None
      with Gpointer.Null -> None

    method private add_entries is_expand entries =
      try
        entries
        |> List.iter begin fun (entry : Merlin_t.entry) ->
          if is_destroyed then raise Exit;
          (* TODO: remove duplicates *)
          let row = model#append () in
          model#set ~row ~column:col_is_exp is_expand;
          model#set ~row ~column:col_kind (icon_of_kind entry.kind);
          model#set ~row ~column:col_name entry.name;
          model#set ~row ~column:col_desc entry.desc;
          model#set ~row ~column:col_info entry.info;
          count <- count + 1;
        end
      with Exit -> ()

    initializer
      view#misc#connect#after#realize ~callback:(fun _ -> vc_name#set_sizing `GROW_ONLY) |> ignore;
      self#misc#connect#destroy ~callback:begin fun () ->
        single_instance := None;
        is_destroyed <- true
      end |> ignore;
      self#connect#first_entry_available ~callback:begin fun () ->
        Gmisclib.Idle.add (fun () -> self#select_row `NEXT; (* Select first result *));
      end |> ignore;
      self#connect#loading_complete ~callback:begin fun count ->
        if count > 0 then Gmisclib.Idle.add self#show_info
        else self#destroy()
      end |> ignore;
      lview#selection#connect#changed ~callback:self#show_info |> ignore;

    method connect = new signals ~first_entry_available ~loading_complete
  end

and first_entry_available () = object inherit [unit] signal () end
and loading_complete () = object inherit [int] signal () end
and signals ~first_entry_available ~loading_complete =
  object
    inherit ml_signals [first_entry_available#disconnect; loading_complete#disconnect]
    method first_entry_available = first_entry_available#connect ~after
    method loading_complete = loading_complete#connect ~after
  end

let create_window ~project ~page =
  let view = page#ocaml_view in
  let x, y = view#get_location_at_cursor () in
  let compl = new widget ~project ~page ~x ~y () in
  create ~compl:(compl :> completion) ~x ~y ~project ~page
