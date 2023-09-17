open Merlin

class widget ~project ~(page : Editor_page.page) ~x ~y ?packing () =
  let rec string_intersect left right =
    let len_right = String.length right in
    if len_right = 0 then String.length left, 0
    else
      let re = Str.regexp_string right in
      try
        Str.search_backward re left (String.length left),
        len_right
      with Not_found ->
        string_intersect left (Str.first_chars right (len_right - 1))
  in
  let markup_types_color = Lexical_markup.parse Preferences.preferences#get in
  let markup_types_bw = Print_type.markup2 in
  let vbox = GPack.vbox ~spacing:5 ~border_width:0 ?packing () in
  let cols = new GTree.column_list in
  let col_kind  = cols#add Gobject.Data.string in
  let col_name  = cols#add Gobject.Data.string in
  let col_desc  = cols#add Gobject.Data.string in
  let col_info  = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let renderer = GTree.cell_renderer_text [`SCALE `SMALL; `YPAD 0] in
  let renderer_desc = GTree.cell_renderer_text [`SCALE `SMALL; `FONT (Preferences.preferences#get.editor_base_font)] in
  let vc_kind = GTree.view_column ~renderer:(renderer, ["markup", col_kind]) () in
  let vc_name = GTree.view_column ~renderer:(renderer, ["markup", col_name]) () in
  let vc_descr = GTree.view_column ~renderer:(renderer_desc, ["markup", col_desc]) () in
  let label_prefix = GMisc.label ~packing:vbox#add ~show:false () in
  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let lview = GTree.view ~model:model ~headers_visible:false ~reorderable:false ~height:200 ~packing:sw#add () in
  let _ = lview#set_search_column 1 in
  let _ = lview#append_column vc_kind in
  let _ = lview#append_column vc_name in
  let view = page#ocaml_view in
  let buffer = view#buffer in
  let position = buffer#get_iter_at_mark `INSERT in
  let start, _ = page#buffer#as_text_buffer#select_word ~pat:Ocaml_word_bound.longid ~select:false ~search:false () in
  let prefix = page#buffer#get_text ~start ~stop:position () in
  object (self)
    inherit GObj.widget vbox#as_widget
    method complete (window : GWindow.window) =
      let merlin func =
        let filename = match (view#obuffer#as_text_buffer :> Text.buffer)#file with Some file -> file#filename | _ -> "" in
        let source_code = buffer#get_text () in
        func ~filename ~source_code
      in
      (*      let prefix =
              Merlin.enclosing
      *)      label_prefix#set_text prefix;
      let add_entries kind entries =
        entries
        |> List.iter begin fun (entry : Merlin_t.entry) ->
          Gmisclib.Idle.add begin fun () ->
            let row = model#append () in
            model#set ~row ~column:col_kind kind;
            model#set ~row ~column:col_name entry.name;
            model#set ~row ~column:col_desc entry.desc;
            model#set ~row ~column:col_info entry.info;
          end
        end
      in
      merlin@@complete_prefix ~position ~prefix begin fun complete_prefix ->
        (*        match complete_prefix.entries with
                  | [] ->*)
        merlin@@expand_prefix ~position ~prefix begin fun expand_prefix ->
          GtkThread.async (fun () -> expand_prefix.entries |> add_entries "E") ()
        end;
        (*| _ ->*)
        GtkThread.async (fun () -> complete_prefix.entries |> add_entries "C") ();
      end;
      Gmisclib.Idle.add (fun () -> window#move ~x ~y);
      lview#connect#row_activated ~callback:begin fun path _ ->
        let row = model#get_iter path in
        let name = model#get ~row ~column:col_name in
        let a, b = string_intersect prefix name in
        buffer#insert_interactive (String.sub name b (String.length name - b)) |> ignore;
        window#destroy()
      end |> ignore;

    initializer
      view#misc#connect#after#realize ~callback:(fun _ -> vc_name#set_sizing `GROW_ONLY) |> ignore;
      let label_info = GMisc.label ~markup:"" ~xalign:0.0 ~yalign:0.0 ~xpad:8 ~ypad:8 () in
      label_info#set_use_markup true;
      let window_info = Gtk_util.window label_info#coerce ~focus:false ~type_hint:`MENU ~parent:lview ~x ~y ~show:false () in
      self#misc#connect#destroy ~callback:(fun () -> window_info#destroy()) |> ignore;
      lview#selection#connect#changed ~callback:begin fun () ->
        match lview#selection#get_selected_rows with
        | [] -> ()
        | path :: _ ->
            let row = model#get_iter path in
            let name = model#get ~row ~column:col_name in
            let desc = model#get ~row ~column:col_desc in
            let info = model#get ~row ~column:col_info in
            let font_name = Preferences.preferences#get.editor_base_font in
            let family = String.sub font_name 0 (Option.value (String.rindex_opt font_name ' ') ~default:(String.length font_name)) in
            let markup =
              Printf.sprintf "<span size='small' font_family='%s'>%s</span>\n<span size='small'>%s</span>"
                family (markup_types_bw desc) (Glib.Markup.escape_text info)
            in
            label_info#set_label markup;
            let x = x + self#misc#allocation.Gtk.width in
            window_info#resize ~width:1 ~height:1;
            window_info#show();
            Gmisclib.Idle.add (fun () -> window_info#move ~x ~y);
            let a, b = string_intersect prefix name in
            label_prefix#set_label (Printf.sprintf "%s (%d, %d)" prefix a b)
      end |> ignore;
  end

let create_window ~project ~page =
  let view = page#ocaml_view in
  let x, y = view#get_location_at_cursor () in
  let compl = new widget ~project ~page ~x ~y () in
  let window = Gtk_util.window compl#coerce ~parent:view ~x ~y () in
  compl#complete window;
  compl
