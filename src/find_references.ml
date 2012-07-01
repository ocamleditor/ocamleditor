open Printf
open GUtil
open Annotation
open Definition
open Miscellanea

type get_page = string -> Editor_page.page
type goto_page = Editor_page.page -> unit

type source = [
  | `DEF_PAGE_ITER of Editor_page.page * GText.iter * get_page * goto_page
  | `DEF_FILE_POS of string * int * get_page * goto_page
  | `EXT of string * ((string * ref_pos list) list) * get_page * goto_page
]

type result = {
  filename : string;
  get_page : get_page;
  mutable page : Editor_page.page option;
  goto_page : goto_page;
  mutable offset : ((int * int) * (int * int));
  mutable mark : (Gtk.text_mark * Gtk.text_mark) option;
}

let new_mark_name = let i = ref 0 in fun () -> incr i; sprintf "find_references_%d" !i

class widget ~editor ?packing () =
  let search_started = new search_started () in
  let search_finished = new search_finished () in
  let vbox = GPack.vbox ?packing () in
  let columns = new GTree.column_list in
  let col_text = columns#add Gobject.Data.string in
  let col_result = columns#add Gobject.Data.caml_option in
  let model = GTree.tree_store columns in
  let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let view = GTree.view ~model ~headers_visible:false ~packing:sw#add () in
  let renderer = GTree.cell_renderer_text [] in
  let vc_text = GTree.view_column ~renderer:(renderer, ["markup", col_text]) () in
  let iter_of_linechar (page : Editor_page.page) lnum cnum =
    let start = page#buffer#get_iter (`LINE (lnum - 1)) in
    let pos = Convert.offset_from_pos (page#buffer#get_text ~start ~stop:start#forward_to_line_end ()) ~pos:cnum in
    start#forward_chars pos
  in
  let markup line = "<tt>" ^ (Glib.Markup.escape_text (Miscellanea.trim line)) ^ "</tt>" in
  (** Destroy marks in buffers *)
  let destroy_marks () =
    model#foreach begin fun path row ->
      begin
        match model#get ~row ~column:col_result with
          | Some result ->
            begin
              match result.mark with
                | Some (m1, m2) ->
                  let page = match result.page with Some x -> x | _ -> assert false in
                  GtkText.Buffer.delete_mark page#buffer#as_buffer m1;
                  GtkText.Buffer.delete_mark page#buffer#as_buffer m2;
                  result.mark <- None
                | None -> ()
            end
          | _ -> ()
      end;
      false
    end
  in
  (** Selection changed *)
  let selection_changed ?path ~grab_focus () =
    try
      let path = match path with Some x -> x | _ -> List.hd view#selection#get_selected_rows in
      let row = model#get_iter path in
      begin
        match model#get ~row ~column:col_result with
          | None -> ()
          | Some result ->
            let page = match result.page with
              | None ->
                let page = result.get_page result.filename in
                editor#connect#remove_page ~callback:begin fun removed ->
                  if removed#view#misc#get_oid = page#view#misc#get_oid then begin
                    destroy_marks();
                    result.page <- None;
                    result.mark <- None;
                  end
                end;
                result.page <- Some page;
                page
              | Some page -> page
            in
            result.goto_page page;
            let start, stop = match result.mark with
              | None ->
                let (a, b), (c, d) = result.offset in
                let start = iter_of_linechar page a b in
                let stop = iter_of_linechar page c d in
                let name = new_mark_name() in
                let start = page#buffer#create_mark ~name start in
                let name = new_mark_name() in
                let stop = page#buffer#create_mark ~name stop in
                result.mark <- Some (start, stop);
                start, stop
              | Some (start, stop) -> start, stop
            in
            let start = page#buffer#get_iter_at_mark (`MARK start) in
            let stop = page#buffer#get_iter_at_mark (`MARK stop) in
            page#view#scroll_lazy start;
            page#buffer#select_range start stop;
            if grab_focus || page#view#misc#get_flag `HAS_FOCUS then
              Gmisclib.Idle.add page#view#misc#grab_focus;
      end;
    with Failure "hd" -> ()
  in
  let _ =
    view#append_column vc_text;
    view#misc#set_property "enable-tree-lines" (`BOOL true);
    view#selection#set_mode `SINGLE;
    view#selection#connect#after#changed ~callback:(fun () -> selection_changed ~grab_focus:false ());
    (** Row activated *)
    view#connect#row_activated ~callback:begin fun path _ ->
      view#expand_row path;
      begin
        match model#get ~row:(model#get_iter path) ~column:col_result with
          | None ->
            let path = model#get_path (model#iter_children ~nth:0 (Some (model#get_iter path))) in
            selection_changed ~path ~grab_focus:true()
          | Some _ -> ()
      end;
      if view#misc#get_flag `HAS_FOCUS then (selection_changed ~grab_focus:true()) else (view#misc#grab_focus ());
    end;
    (** Paint roots background with a different color *)
    vc_text#set_cell_data_func renderer begin fun model row ->
      let path = model#get_path row in
      let depth = GTree.Path.get_depth path in
       renderer#set_properties [
         `CELL_BACKGROUND Oe_config.find_references_title_bgcolor;
         `FOREGROUND Oe_config.find_references_title_fgcolor;
         `CELL_BACKGROUND_SET (depth = 1);
         `FOREGROUND_SET (depth = 1);
         ]
    end;
    (** Destroy view *)
    view#connect#destroy ~callback:destroy_marks;
  in
object (self)
  inherit GObj.widget vbox#as_widget
  inherit Messages.page

  val mutable signalid_row_expanded = None

  method parent_changed messages = ()

  initializer
    signalid_row_expanded <-
      Some (view#connect#after#row_expanded ~callback:begin fun row path ->
        let area = view#get_cell_area ~path () in
        let y = view#vadjustment#value +. float (Gdk.Rectangle.y area) in
        Gmisclib.Idle.add ~prio:600 (fun () ->
          view#vadjustment#set_value (min y (view#vadjustment#upper -. view#vadjustment#page_size)));
      end);

  method private set_root_text ~root ~filename ~name =
    model#set ~row:root ~column:col_text (sprintf "<b>References to identifier </b><tt>%s</tt><b> in module </b><tt>%s</tt>"
      name (modname_of_path filename));
    Gaux.may signalid_row_expanded ~f:view#misc#handler_block;
    view#expand_row (model#get_path root);
    Gaux.may signalid_row_expanded ~f:view#misc#handler_unblock;


  method private append_row_definition ~root ~filename =
    let definition = model#append ~parent:root () in
    model#set ~row:definition ~column:col_text (sprintf "Definition (%s)" (Filename.basename filename));
    let usages = model#append ~parent:root () in
    model#set ~row:usages ~column:col_text "Direct usages";
    definition, usages

  method private append_row_line ~parent ~page ~goto ~filename ~line ~start ~stop =
    let result = {
      filename = filename;
      get_page = page;
      page = None;
      goto_page = goto;
      offset = (start, stop);
      mark = None;
    } in
    let row = model#append ~parent () in
    model#set ~row ~column:col_text (markup line);
    model#set ~row ~column:col_result (Some result);

  method private append_row_file ~parent ~filename ~page ~goto ~references =
    let row = model#append ~parent () in
    model#set ~row ~column:col_text (Filename.basename filename);
    List.iter begin fun (name, ((a, _) as rstart), rstop) ->
      let line = Miscellanea.get_line_from_file ~filename a in
      self#append_row_line ~parent:row ~page ~goto ~filename ~line ~start:rstart ~stop:rstop;
    end references;

  method private append_external ~parent ~get_page ~goto ref_ext =
    List.iter begin fun (filename, references) ->
      self#append_row_file ~parent ~filename ~page:get_page ~goto ~references
    end ref_ext

  method find ~(project : Project_type.t) (source : source) =
    search_started#call();
    let root = model#append () in
    let usages = ref root in
    let definition = ref root in
    begin
      match source with
        | `DEF_FILE_POS (filename, offset, page, goto) ->
          let int_refs = Definition.find_references ~src_path:(Project.path_src project) ~project ~filename ~offset () in
          begin
            match int_refs with
              | None -> ()
              | Some result ->
                (** Definition *)
                let name, ((a, b) as rstart), ((c, d) as rstop) = result.ref_def in
                self#set_root_text ~root ~filename ~name;
                let def_node, usages_node = self#append_row_definition ~root ~filename in
                usages := usages_node;
                definition := def_node;
                (* line of the definition *)
                let line = Miscellanea.get_line_from_file ~filename a in
                self#append_row_line ~parent:def_node ~page ~goto ~line ~start:rstart ~stop:rstop ~filename;
                Gaux.may signalid_row_expanded ~f:view#misc#handler_block;
                view#expand_row (model#get_path def_node);
                Gaux.may signalid_row_expanded ~f:view#misc#handler_unblock;
                (** Internal references *)
                if result.ref_int <> [] then begin
                  self#append_row_file ~parent:!usages ~filename ~page ~goto ~references:result.ref_int
                end;
                (** External references *)
                self#append_external ~parent:!usages ~get_page:page ~goto result.ref_ext
          end
        | `DEF_PAGE_ITER (page, iter, get_page, goto) ->
          let filename = page#get_filename in
          let offset = Glib.Utf8.offset_to_pos (page#buffer#get_text ()) ~pos:0 ~off:iter#offset in
          let int_refs = Definition.find_references ~src_path:(Project.path_src project) ~project ~filename ~offset () in
          begin
            match int_refs with
              | None -> ()
              | Some result ->
                let iter_of_linechar = iter_of_linechar page in
                (** Definition *)
                let name, ((a, b) as rstart), ((c, d) as rstop) = result.ref_def in
                self#set_root_text ~root ~filename ~name;
                let def_node, usages_node = self#append_row_definition ~root ~filename in
                usages := usages_node;
                definition := def_node;
                (* line of the definition *)
                let iter = iter_of_linechar a 0 in
                let line = iter#get_text ~stop:iter#forward_to_line_end in
                self#append_row_line ~parent:def_node ~page:(fun _ -> page) ~goto ~line ~start:rstart ~stop:rstop ~filename;
                Gaux.may signalid_row_expanded ~f:view#misc#handler_block;
                view#expand_row (model#get_path def_node);
                Gaux.may signalid_row_expanded ~f:view#misc#handler_unblock;
                (** Internal references *)
                if result.ref_int <> [] then begin
                  self#append_row_file ~parent:!usages ~filename ~page:(fun _ -> page) ~goto ~references:result.ref_int
                end;
                (** External references *)
                self#append_external ~parent:!usages ~get_page ~goto result.ref_ext
          end;
        | `EXT (fullname, ref_ext, get_page, goto) ->
          model#set ~row:root ~column:col_text (sprintf "<b>References to </b><tt>%s</tt>" fullname);
          self#append_external ~parent:!usages ~get_page ~goto ref_ext;
    end;
    Gaux.may signalid_row_expanded ~f:view#misc#handler_block;
    view#expand_row (model#get_path root);
    view#expand_row (model#get_path !definition);
    view#expand_row (model#get_path !usages);
    if model#iter_n_children (Some !usages) = 1 then (view#expand_row (model#get_path (model#iter_children ~nth:0 (Some !usages))));
    Gaux.may signalid_row_expanded ~f:view#misc#handler_unblock;
    search_finished#call();
    Gmisclib.Idle.add (fun () -> view#scroll_to_cell ~align:(0.0, 0.0) (model#get_path root) vc_text);

    method connect = new signals ~search_started ~search_finished
end

and search_started () = object (self) inherit [unit] signal () as super end
and search_finished () = object (self) inherit [unit] signal () as super end
and signals ~search_started ~search_finished =
object (self)
  inherit ml_signals [search_started#disconnect; search_finished#disconnect]
  method search_started = search_started#connect ~after
  method search_finished = search_finished#connect ~after
end
