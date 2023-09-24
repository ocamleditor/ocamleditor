open Merlin
open GUtil
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

let single_instance = ref None

let icon_of_kind = function
  | "Value" -> " " (*   󰊕 *)
  | "Type" -> "𝚻" (* 𝛕 *)
  | "Module" -> " " (*     󰆧          󰆦          *)
  | "Constructor" -> " "
  | "Label" -> "󰌖 "
  | "Class" -> " "
  | "Method" -> " "
  | "#" -> " " (* 󰽭 *)
  | x -> x

class widget ~project ~(page : Editor_page.page) ~x ~y ?packing () =
  let markup_types_color = Lexical_markup.parse Preferences.preferences#get in
  let markup_types_bw = Print_type.markup2 in
  let vbox = GPack.vbox ~spacing:5 ~border_width:0 ?packing () in
  let cols = new GTree.column_list in
  let col_data  = cols#add Gobject.Data.string in
  let col_kind  = cols#add Gobject.Data.string in
  let col_name  = cols#add Gobject.Data.string in
  let col_desc  = cols#add Gobject.Data.string in
  let col_info  = cols#add Gobject.Data.string in
  let model = GTree.list_store cols in
  let renderer = GTree.cell_renderer_text [`SCALE `SMALL; `YPAD 0] in
  let vc_kind = GTree.view_column ~renderer:(renderer, ["text", col_kind]) () in
  let vc_name = GTree.view_column ~renderer:(renderer, ["text", col_name]) () in
  let label_prefix = GMisc.label ~packing:vbox#add ~show:false () in
  let sw = GBin.scrolled_window ~shadow_type:`NONE ~hpolicy:`NEVER ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let lview = GTree.view ~model ~headers_visible:false ~reorderable:false ~height:200 ~packing:sw#add () in
  let _ = lview#set_enable_search false in
  let _ = lview#set_search_column 1 in
  let _ = lview#append_column vc_kind in
  let _ = lview#append_column vc_name in
  let _ = lview#selection#set_mode `SINGLE in
  let label_info = GMisc.label ~markup:"" ~xalign:0.0 ~yalign:0.0 ~xpad:8 ~ypad:8 () in
  let window_info = Gtk_util.window_tooltip label_info#coerce ~parent:page ~x ~y () in
  (*let _ = lview#misc#modify_base [`ACTIVE, `NAME ?? (Preferences.preferences#get.editor_bg_color_popup)] in*)
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
  object (self)
    inherit GObj.widget vbox#as_widget

    val mutable current_prefix = ""
    val mutable count = 0
    val first_entry_available = new first_entry_available()
    val loading_complete = new loading_complete()
    val mutable is_destroyed = false

    method complete (window : GWindow.window) =
      let position = buffer#get_iter_at_mark `INSERT in
      let word_start, _ = page#buffer#as_text_buffer#select_word ~pat:Ocaml_word_bound.longid ~select:false ~search:false () in
      let is_sharp uc = Glib.Utf8.from_unichar uc = "#" in
      let start = position#backward_find_char ~limit:word_start is_sharp in
      let is_method_compl = is_sharp start#char in
      let start = if is_method_compl then start#forward_char else start in
      let prefix = page#buffer#get_text ~start ~stop:position () in
      current_prefix <- prefix;
      (*      let prefix =
              Merlin.enclosing
      *)
      label_prefix#set_text prefix;
      count <- 0;
      self#invoke_merlin ~prefix ~position ~expand:(not is_method_compl) window;
      let view_signals =
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
              |> Option.iter begin fun path ->
                let row = model#get_iter path in
                let name = model#get ~row ~column:col_name in
                self#apply name;
                self#destroy()
              end;
              true
            end else false
          end;
          view#event#connect#focus_out ~callback:(fun _ -> self#destroy(); false);
        ]
      in
      self#misc#connect#destroy ~callback:begin fun () ->
        view_signals |> List.iter (GtkSignal.disconnect view#as_view);
        window#destroy();
      end |> ignore;
      window#move ~x ~y

    method private invoke_merlin ~prefix ~position ?(expand=true) window =
      merlin@@complete_prefix ~position ~prefix begin fun complete_prefix ->
        window#show();
        begin
          match complete_prefix.entries with
          | first :: others ->
              [first] |> self#add_entries "C";
              first_entry_available#call();
              others |> self#add_entries "C";
          | [] -> ()
        end;
        if expand then begin
          merlin@@expand_prefix ~position ~prefix begin fun expand_prefix ->
            expand_prefix.entries |> self#add_entries "E";
            loading_complete#call count;
            (*merlin@@list_modules begin fun modules ->
              modules |> String.concat ", " |> Printf.printf "%s"
              end*)
          end
        end else
          loading_complete#call count;
      end;

    method private apply name =
      let a, b = String_utils.locate_intersection current_prefix name in
      let substitute = Str.string_after name b in
      Printf.printf "name=%S current_prefix=%S %d %d %S\n%!" name current_prefix a b substitute;
      let start = buffer#get_iter `INSERT in
      let _, stop = page#buffer#as_text_buffer#select_word ~pat:Ocaml_word_bound.regexp ~select:false ~search:false () in
      let stop = if start#compare stop >= 0 then start else stop in
      page#view#tbuffer#undo#begin_block ~name:"compl";
      buffer#delete_interactive ~start ~stop () |> ignore;
      buffer#insert_interactive substitute |> ignore;
      page#view#tbuffer#undo#end_block();
      (*buffer#get_text() |> Lex.paths_opened |> String.concat ";" |> Printf.printf "%s\n%!" ;*)

    method private select_row direction =
      let path =
        match lview#selection#get_selected_rows with
        | [] -> GTree.Path.create [0]
        | [ path ] when direction = `NEXT -> GTree.Path.next path; path
        | [ path ] when direction = `PREV -> if GTree.Path.prev path then path else GTree.Path.create [0]
        | _ -> assert false
      in
      lview#selection#select_path path;
      lview#scroll_to_cell path vc_kind

    method private move_info path =
      let row_area = lview#get_cell_area ~path () in
      let x = x + self#misc#allocation.Gtk.width in
      let y = y + Gdk.Rectangle.y row_area in
      window_info#resize ~width:1 ~height:1;
      Gmisclib.Idle.add (fun () -> window_info#move ~x ~y);

    method private show_info () =
      match lview#selection#get_selected_rows with
      | [] -> ()
      | path :: _ ->
          let row = model#get_iter path in
          let name = model#get ~row ~column:col_name in
          let desc = model#get ~row ~column:col_desc in
          let info = model#get ~row ~column:col_info in
          if String.trim desc <> "" || String.trim info <> "" then begin
            Gmisclib.Idle.add begin fun () ->
              let markup =
                Printf.sprintf "<span size='small' font_family='%s'>%s</span>\n<span size='small'>%s</span>"
                  font_family_monospace (markup_types_bw desc) (Glib.Markup.escape_text info)
              in
              label_info#set_label markup;
              self#move_info path;
              window_info#show();
            end;
          end

    method private selected_path =
      try
        match lview#selection#get_selected_rows with
        | [path] -> Some path
        | _ -> None
      with Gpointer.Null -> None

    method private add_entries data entries =
      try
        entries
        |> List.iter begin fun (entry : Merlin_t.entry) ->
          if is_destroyed then raise Exit;
          (* TODO: remove duplicates *)
          let row = model#append () in
          model#set ~row ~column:col_data data;
          model#set ~row ~column:col_kind (icon_of_kind entry.kind);
          model#set ~row ~column:col_name entry.name;
          model#set ~row ~column:col_desc entry.desc;
          model#set ~row ~column:col_info entry.info;
          count <- count + 1;
          Gmisclib.Idle.add (fun () -> self#selected_path |> Option.iter self#move_info)
        end
      with Exit -> ()

    initializer
      window_info#misc#hide();
      view#misc#connect#after#realize ~callback:(fun _ -> vc_name#set_sizing `GROW_ONLY) |> ignore;
      label_info#set_use_markup true;
      (*let window_info = Gtk_util.window label_info#coerce ~focus:false ~type_hint:`MENU ~parent:page ~x ~y ~show:false () in*)
      self#misc#connect#destroy ~callback:begin fun () ->
        window_info#destroy();
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
      lview#selection#connect#changed ~callback:begin fun () ->
        self#show_info ()
      end|> ignore;

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
  begin
    match !single_instance with
    | Some instance -> instance#destroy();
    | _ -> ()
  end;
  let view = page#ocaml_view in
  let x, y = view#get_location_at_cursor () in
  let compl = new widget ~project ~page ~x ~y () in
  (*let window = Gtk_util.window compl#coerce ~parent:page ~focus:false ~destroy_on_focus_out:false ~x ~y () in*)
  let window = Gtk_util.window_tooltip compl#coerce ~parent:page ~x ~y () in
  window#misc#hide();
  compl#complete window;
  single_instance := Some compl;
