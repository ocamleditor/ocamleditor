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

open GUtil
open Unix
open Miscellanea
open Project
open Printf
open Preferences

let set_menu_item_nav_history_sensitive = ref (fun () -> failwith "set_menu_item_nav_history_sensitive")

(** Editor *)
class editor () =
  let hpaned = GPack.paned `HORIZONTAL () in
  let notebook = GPack.notebook ~tab_border:0 ~show_border:false
    ~packing:(hpaned#pack2 ~resize:true ~shrink:true) ~scrollable:true () in
  let _ = hpaned#set_position 250 in
  let incremental_search = new Incremental_search.incremental () in
  let switch_page = new switch_page () in
  let remove_page = new remove_page () in
  let modified_changed = new modified_changed () in
  let changed = new changed () in
  let add_page = new add_page () in
object (self)
  inherit GObj.widget hpaned#as_widget
  val mutable closing = false
  val mutable pages = []
  val mutable pages_cache = []
  val mutable project = Project.create ~filename:"untitled.xyz" ()
  val mutable menu_items = []
  val liim_delim = Liim.create ~delay:1.0 ()
  val liim_fast = Liim.create ~delay:0.3 ()
  val location_history = Location_history.create()
  val mutable file_history =
    File_history.create
      ~filename:Oe_config.file_history_filename
      ~max_length:Oe_config.file_history_max_length
  val mutable last_active_page = None
  val code_folding_enabled = new GUtil.variable false
  val show_global_gutter = new GUtil.variable false
  val mutable show_whitespace_chars = !Preferences.preferences.Preferences.pref_show_whitespace_chars
  val mutable word_wrap = !Preferences.preferences.Preferences.pref_editor_wrap
  val mutable show_outline = true
  val mutable bookmark_view = None

  method pack_outline widget =
    if show_outline then begin
      (try hpaned#remove hpaned#child1 with Gpointer.Null -> ());
      hpaned#pack1 ~resize:false ~shrink:true widget;
    end

  method show_outline = show_outline
  method set_show_outline show =
    show_outline <- show;
    try
      if show then begin
        self#with_current_page begin fun page ->
          match page#outline with
            | Some ol -> self#pack_outline ol#coerce
            | _ ->
              (try hpaned#remove hpaned#child1 with Gpointer.Null -> ());
              page#compile_buffer ~commit:false ()
        end;
      end else (try hpaned#remove hpaned#child1 with Gpointer.Null -> ());
    with Gpointer.Null -> ()

  method show_whitespace_chars = show_whitespace_chars
  method set_show_whitespace_chars x =
    show_whitespace_chars <- x;
    List.iter begin fun page ->
      page#view#set_show_whitespace_chars x;
      GtkBase.Widget.queue_draw page#view#as_widget
    end (pages @ (snd (List.split pages_cache)))

  method word_wrap = word_wrap
  method set_word_wrap x =
    word_wrap <- x;
    List.iter begin fun page ->
      page#view#set_word_wrap x;
      GtkBase.Widget.queue_draw page#view#as_widget
    end (pages @ (snd (List.split pages_cache)))

  method with_current_page f = match self#get_page Oe.Page_current with Some page -> f page | _ -> ()

  method code_folding_enabled = code_folding_enabled
  method show_global_gutter = show_global_gutter

  method file_history = file_history
  method location_history = location_history

  method set_menu m = menu_items <- m

  method project = project
  method set_project proj = project <- proj

  method tab_pos = notebook#tab_pos

  method show_tabs = notebook#show_tabs
  method set_show_tabs x =
    notebook#set_show_tabs x;
    if x then (notebook#misc#grab_focus())

  method pages = List.rev pages

  method goto_view v =
    match self#get_page (Oe.Page_view v) with
      | None -> ()
      | Some page ->
        if not page#load_complete then (self#load_page page);
        notebook#goto_page (notebook#page_num page#coerce);

  method get_page : Oe.editor_page_id -> Editor_page.page option = fun which ->
    try
      let page = match which with
        | Oe.Page_current when closing -> raise Not_found
        | Oe.Page_current -> List.find begin fun p ->
            p#get_oid = (notebook#get_nth_page notebook#current_page)#get_oid
          end pages
        | Oe.Page_file file -> List.find begin fun p ->
            match p#file with None -> false | Some f -> f#path = file#path
          end pages
        | Oe.Page_num n -> List.find (fun p -> p#get_oid = (notebook#get_nth_page n)#get_oid) pages
        | Oe.Page_view v -> List.find (fun p -> p#view#get_oid = v#get_oid) pages
      in Some page
    with Not_found -> None

  method dialog_file_open () = Editor_dialog.file_open ~editor:self ()

  method switch_mli_ml (page : Editor_page.page) =
    let filename = page#get_filename in
    if Filename.check_suffix filename ".ml" then begin
      let mli = (Filename.chop_extension filename) ^ ".mli" in
      self#load_mli page#project mli
    end else if Filename.check_suffix filename ".mli" then begin
      let ml = (Filename.chop_extension filename) ^ ".ml" in
      ignore (self#open_file ~active:true ~offset:0 ml)
    end

  method private load_mli proj filename =
    if Sys.file_exists filename then (ignore (self#open_file ~active:true ~offset:0 filename))
    else begin
      Dialog.confirm
        ~message:("File\n\""^filename^"\"\ndoes not exist, create?")
        ~f:(begin fun () ->
          let ml = (Filename.chop_extension filename) ^ ".ml" in
          let old = Sys.getcwd () in
          Sys.chdir (Filename.dirname filename);
          let includes = Project.get_includes proj in
          let cmd = (sprintf "ocamlc -i -w a -thread %s %s > %s"
            (String.concat " " (List.map (fun x -> "-I " ^ x) includes))
            (Filename.basename ml) (Filename.basename filename)) in
          ignore (Sys.command cmd);
          Sys.chdir old;
          self#load_mli proj filename
        end, fun () -> ()) self;
    end

  method location_history_add ~iter ~kind () =
    self#with_current_page begin fun page ->
      Location_history.add location_history
        ~kind ~view:(page#view :> GText.view)
        ~filename:page#get_filename ~offset:iter#offset;
      Gmisclib.Idle.add ~prio:600 !set_menu_item_nav_history_sensitive
    end

  method location_history_goto location =
    let filename = location.Location_history.filename in
    if Sys.file_exists filename then begin
      match self#get_page (Oe.Page_file (File.create filename ())) with
        | None ->
          self#open_file ~active:true ~offset:0 filename;
          self#location_history_goto location
        | Some page ->
          let view = (page#view :> Text.view) in
          self#goto_view view;
          let where =
            match location.Location_history.mark with
              | Some mark -> page#buffer#get_iter_at_mark (`MARK mark)
              | None ->
                let offset = location.Location_history.offset in
                (* Now that the page is loaded, marks are used in place of offsets. *)
                location.Location_history.mark <-
                  (Some (Location_history.create_mark ~buffer:(view :> GText.view)#buffer ~offset));
                page#buffer#get_iter (`OFFSET offset);
          in
          view#buffer#place_cursor where;
          ignore (view#scroll_lazy where);
          view#misc#grab_focus();
    end

  method bookmark_create ~num =
    self#with_current_page begin fun page ->
      let filename = page#get_filename in
      let mark = page#buffer#create_mark (page#buffer#get_iter `INSERT) in
      let old_marker =
        try (List.find (fun bm -> bm.Bookmark.num = num) Bookmark.bookmarks#get).Bookmark.marker
        with Not_found -> None
      in
      Gaux.may old_marker ~f:(fun old -> Gutter.destroy_markers page#view#gutter [old]);
      let marker = Gutter.create_marker ~kind:(`Bookmark num) ~mark ~pixbuf:(List.assoc num Bookmark.icons) () in
      Bookmark.create ~num ~filename ~mark ~marker ();
      page#view#gutter.Gutter.markers <- marker :: page#view#gutter.Gutter.markers;
      page#view#paint_gutter();
    end

  method bookmark_goto ~num =
    try
      let bm = List.find (fun bm -> bm.Bookmark.num = num) Bookmark.bookmarks#get in
      match self#get_page (Oe.Page_file (File.create bm.Bookmark.filename ())) with
        | None ->
          let _ = self#open_file ~active:true ~offset:0 bm.Bookmark.filename in
          Gmisclib.Idle.add ~prio:300 (fun () -> self#bookmark_goto ~num)
        | Some page ->
          if not page#view#realized then (self#goto_view page#view);
          Bookmark.apply bm begin function
            | `OFFSET offset ->
              let _ = Bookmark.offset_to_mark (page#buffer :> GText.buffer) bm in
              self#bookmark_goto ~num;
            | `ITER it ->
              let where = new GText.iter it in
              page#view#scroll_lazy where;
              page#buffer#place_cursor ~where;
              page#view#misc#grab_focus();
          end;
          if page#view#realized then (Gmisclib.Idle.add (*~prio:300*) (fun () -> self#goto_view page#view));
          Gmisclib.Idle.add ~prio:300 Bookmark.write;
    with Not_found -> ()

  method scroll_to_definition iter =
    self#with_current_page begin fun page ->
      match Definition.find_definition ~project:self#project ~page ~iter with
        | None -> ()
        | Some (_, _, filename, start, stop) ->
          self#location_history_add ~iter ~kind:`BROWSE ();
          let rec get_page () =
            match self#get_page (Oe.Page_file (File.create filename ())) with
              | Some page -> page
              | None ->
                self#open_file ~active:true ~offset:0 filename;
                get_page ()
          in
          let page = get_page() in
          let view = (page#view :> Text.view) in
          self#goto_view view;
          let start = page#buffer#get_iter (`OFFSET start) in
          let stop = page#buffer#get_iter (`OFFSET stop) in
          page#buffer#select_range start stop;
          page#view#scroll_lazy start;
          self#location_history_add ~iter:start ~kind:(`BROWSE : Location_history.kind) ();
    end

  method find_references (iter : GText.iter) =
    self#with_current_page begin fun page ->
      let rec get_page filename =
        match self#get_page (Oe.Page_file (File.create filename ())) with
          | Some page ->
            if not page#load_complete then (self#load_page page);
            page
          | None ->
            self#open_file ~active:true ~offset:0 filename;
            get_page filename
      in
      let goto_page = (fun page -> self#goto_view (page#view :> Text.view)) in
      match Definition.find_definition ~project:self#project ~page ~iter with
        | None ->
          begin
            match page#buffer#get_annot iter with
              | None -> ()
              | Some { Oe.annot_annotations = annot_annotations } ->
                Gaux.may (Annotation.get_ext_ref annot_annotations) ~f:begin fun fullname ->
                  let ext_refs = Definition.find_ext_ref ~project ~src_path:(Project.path_src self#project) (`EXACT fullname) in
                  self#do_find_references (`EXT (fullname, ext_refs, get_page, goto_page));
                end
          end;
        | Some (_, _, filename, start, stop) ->
          let def_source =
            match self#get_page (Oe.Page_file (File.create filename ())) with
              | None ->
                `DEF_FILE_POS (filename, start, get_page, goto_page)
              | Some page ->
                if not page#load_complete then (self#load_page page);
                let text = page#buffer#get_text () in
                let start = Convert.offset_from_pos text ~pos:start in
                let start = page#buffer#get_iter (`OFFSET start) in
                `DEF_PAGE_ITER (page, start, get_page, (goto_page))
          in
          self#do_find_references def_source;
    end

  val mutable find_references_results = None;
  method private do_find_references def_source =
    let finish = ref (fun _ -> ()) in
    let res = match find_references_results with
      | None ->
        let widget = new Find_references.widget ~editor:self () in
        widget#misc#connect#destroy ~callback:begin fun () ->
          find_references_results <- None;
        end;
        widget
      | Some x -> x
    in
    let hbox = GPack.hbox ~spacing:3 () in
    let label = GMisc.label ~packing:hbox#pack () in
    ignore (res#connect#search_started ~callback:begin fun () ->
      if res#misc#parent = None then
        (finish := fst (Messages.messages#append_page "" ~label_widget:hbox#coerce res#coerce));
      label#set_text "Search";
      Messages.messages#present res#coerce;
    end);
    ignore (res#connect#search_finished ~callback:begin fun () ->
      !finish true;
      Messages.messages#present res#coerce;
    end);
    res#find ~project:self#project def_source;
    find_references_results <- Some res;

  method dialog_file_select () = Editor_dialog.file_select ~editor:self ()

  method set_tab_pos ?(page:Editor_page.page option) pos =
    notebook#set_tab_pos pos;
    let angle = match pos with
      | `RIGHT when !preferences.pref_tab_vertical_text -> 270.
      | `LEFT when !preferences.pref_tab_vertical_text -> 90.
      | _ -> 0.
    in
    let pgs = match page with None -> pages | Some p -> [p] in
    List.iter begin fun page ->
      let filename = page#get_filename in
      let (align : GBin.alignment), (button : GButton.button), (label : GMisc.label) = page#tab_widget in
      label#set_label (Editor_page.markup_label filename);
      label#set_angle angle;
      let tbox = match pos with
        | `RIGHT when !preferences.pref_tab_vertical_text ->
          let tbox = GPack.vbox () in
          if button#misc#parent <> None then (button#misc#reparent tbox#coerce) else (tbox#pack button#coerce);
          if label#misc#parent <> None then (label#misc#reparent tbox#coerce) else (tbox#pack label#coerce);
          List.iter (fun child -> child#destroy()) align#children;
          align#add tbox#coerce;
          label#set_xalign 0.5;
          label#set_yalign 0.0;
          tbox
        | `LEFT when !preferences.pref_tab_vertical_text ->
          let tbox = GPack.vbox () in
          if button#misc#parent <> None then (button#misc#reparent tbox#coerce) else (tbox#pack button#coerce);
          if label#misc#parent <> None then (label#misc#reparent tbox#coerce) else (tbox#pack label#coerce);
          List.iter (fun child -> child#destroy()) align#children;
          align#add tbox#coerce;
          label#set_xalign 0.5;
          label#set_yalign 1.0;
          tbox
        | _ ->
          let tbox = GPack.hbox () in
          if button#misc#parent <> None then (button#misc#reparent tbox#coerce) else (tbox#pack button#coerce);
          if label#misc#parent <> None then (label#misc#reparent tbox#coerce) else (tbox#pack label#coerce);
          List.iter (fun child -> child#destroy()) align#children;
          align#add tbox#coerce;
          label#set_xalign 0.0;
          label#set_yalign 0.5;
          tbox
      in
      tbox#set_child_packing ~expand:false ~fill:false button#coerce;
      tbox#set_child_packing ~expand:false ~fill:false label#coerce;
    end pgs;

    method load_page ?(scroll=true) (page : Editor_page.page) =
      if not page#load_complete then begin
        (** Load page *)
        ignore (page#load ~scroll ());
        page#view#set_show_whitespace_chars show_whitespace_chars;
        page#view#set_word_wrap word_wrap;
        (** Insert_text *)
        ignore (page#buffer#connect#after#insert_text ~callback:begin fun iter text ->
          page#ocaml_view#code_folding#scan_folding_points();
          Liim.set liim_fast begin fun () ->
            let iter = page#buffer#get_iter `INSERT in
            if page#buffer#lexical_enabled then begin
              let start, stop =
                match page#view#current_matching_tag_bounds with
                  | [_,d; a,_] ->
                    (page#buffer#get_iter_at_mark (`MARK a)), (page#buffer#get_iter_at_mark (`MARK d))
                  | _ -> ((iter#backward_chars (Glib.Utf8.length text))#set_line_index 0), iter#forward_line
              in
              Lexical.tag page#view#buffer ~start ~stop;
            end;
            page#view#paint_current_line_background iter;
            (*Gmisclib.Idle.add ~prio:500 page#ocaml_view#code_folding#scan_folding_points;*)
            (*page#ocaml_view#code_folding#scan_folding_points ();*)
          end;
          (*page#ocaml_view#code_folding#scan_folding_points ();*)
          Liim.set liim_delim page#view#matching_delim;
          self#location_history_add ~iter ~kind:`EDIT ();
          (*page#buffer#set_matching_delims None;*)
        end);
        (** Delete range *)
        ignore (page#buffer#connect#delete_range ~callback:begin fun ~start ~stop ->
          Liim.set liim_fast begin fun () ->
            let iter = page#buffer#get_iter `INSERT in
            if page#buffer#lexical_enabled then begin
              let start, stop =
                match page#view#current_matching_tag_bounds with
                  | [_,d; a,_] ->
                    (page#buffer#get_iter_at_mark (`MARK a)), (page#buffer#get_iter_at_mark (`MARK d))
                  | _ -> (iter#backward_line#set_line_index 0), iter#forward_to_line_end
              in
              Lexical.tag page#view#buffer ~start ~stop;
            end;
            page#view#paint_current_line_background iter;
            (*(*Gmisclib.Idle.add ~prio:500 *)page#ocaml_view#code_folding#scan_folding_points ();*)
          end;
          page#ocaml_view#code_folding#scan_folding_points ();
          Liim.set liim_delim page#view#matching_delim;
          self#location_history_add ~iter:start ~kind:`EDIT ();
          (*page#buffer#set_matching_delims None;*)
        end);
        (** Paste clipboard *)
        ignore (page#view#connect#paste_clipboard ~callback:begin fun () ->
          match (GData.clipboard (Gdk.Atom.clipboard))#text with
            | None -> ()
            | Some text ->
              if page#view#editable then begin
                GtkSignal.stop_emit();
                page#buffer#delete_interactive ~start:(page#buffer#get_iter `INSERT) ~stop:(page#buffer#get_iter `SEL_BOUND) ();
                page#buffer#insert text;
                (*Liim.set liim_fast begin fun () ->*)
                  let iter = page#buffer#get_iter `INSERT in
                  if page#buffer#lexical_enabled then begin
                    Lexical.tag page#view#buffer
                      ~start:(iter#backward_chars (Glib.Utf8.length text))#backward_line
                      ~stop:iter#forward_line;
                  end;
                  page#view#paint_current_line_background iter;
                (*end*)
              end
        end);
        (**  populate_menu *)
        if List.length menu_items > 0 then self#populate_menu page;
        (*  *)
        modified_changed#call();
        page#update_statusbar();
      end

    method open_file ~active ~offset filename =
      try
        let page =
          try
            let page = List.find (fun p -> p#get_filename = filename) pages in
            notebook#goto_page (notebook#page_num page#coerce);
            page
          with Not_found -> begin
            begin
              try
                let page = List.assoc filename pages_cache in
                pages_cache <- List.remove_assoc filename pages_cache;
                pages <- page :: pages;
                page#misc#show_all();
                if active then (notebook#goto_page (notebook#page_num page#coerce));
                add_page#call page;
                page
              with Not_found -> begin
                let file = File.create filename () in
                let page = new Editor_page.page ~file ~project ~offset ~editor:self () in
                (** Tab Label with close button *)
                let button_close = GButton.button ~relief:`NONE () in
                let image = Icons.create Icons.button_close in
                button_close#event#connect#enter_notify ~callback:begin fun _ ->
                  image#set_pixbuf (if page#buffer#modified then Icons.button_close_hi_b else Icons.button_close_hi);
                  false
                end;
                button_close#event#connect#leave_notify ~callback:begin fun _ ->
                  image#set_pixbuf (if page#buffer#modified then Icons.button_close_b else Icons.button_close);
                  false
                end;
                page#buffer#connect#modified_changed ~callback:begin fun () ->
                  image#set_pixbuf (if page#buffer#modified then Icons.button_close_b else Icons.button_close);
                  modified_changed#call();
                end;
                (** Annot type tooltips *)
                page#view#misc#set_has_tooltip true;
                ignore (page#view#misc#connect#query_tooltip ~callback:begin fun ~x ~y ~kbd tooltip ->
                  if x > page#view#gutter.Gutter.size && y > 10 && y < (Gdk.Rectangle.height page#view#visible_rect) - 10 then begin
                    let f () =
                      let location = page#view#window_to_buffer_coords ~tag:`WIDGET ~x ~y in
                      page#tooltip ~typ:!preferences.Preferences.pref_annot_type_tooltips_enabled location;
                    in
                    if (*true ||*) !preferences.Preferences.pref_annot_type_tooltips_delay = 1 then begin
                      Liim.set liim_delim (GtkThread2.async f);
                    end else (f());
                  end else (page#error_indication#hide_tooltip ~force:false ());
                  false;
                end);
                page#buffer#undo#connect#after#redo ~callback:(fun ~name -> changed#call());
                page#buffer#undo#connect#after#undo ~callback:(fun ~name -> changed#call());
                page#buffer#undo#connect#can_redo_changed ~callback:(fun _ -> changed#call());
                page#buffer#undo#connect#can_undo_changed ~callback:(fun _ -> changed#call());
                page#buffer#connect#after#changed ~callback:changed#call;
                (** Tab menu *)
                let ebox = GBin.event_box () in
                Gmisclib.Util.set_ebox_invisible ebox;
                ignore (ebox#event#connect#button_press ~callback:begin fun ev ->
                  if GdkEvent.Button.button ev = 3 then begin
                    notebook#goto_page (notebook#page_num page#coerce);
                    Gmisclib.Idle.add begin fun () ->
                      let menu = GMenu.menu () in
                      let basename = Filename.basename filename in
                      let item = GMenu.image_menu_item ~label:(sprintf "Close \xC2\xAB%s\xC2\xBB" basename) ~packing:menu#add () in
                      item#set_image (GMisc.image ~stock:`CLOSE ~icon_size:`MENU ())#coerce;
                      item#connect#activate ~callback:(fun () -> self#dialog_confirm_close page);
                      let item = GMenu.image_menu_item ~label:(sprintf "Close All Except \xC2\xAB%s\xC2\xBB" basename) ~packing:menu#add () in
                      item#connect#activate ~callback:(fun () -> self#close_all ~except:page ());
                      let item = GMenu.image_menu_item ~label:(sprintf "Revert \xC2\xAB%s\xC2\xBB" basename) ~packing:menu#add () in
                      item#connect#activate ~callback:(fun () -> self#revert page);
                      let _ = GMenu.separator_item ~packing:menu#add () in
                      let item = GMenu.image_menu_item ~label:"Switch to Implementation/Interface" ~packing:menu#add () in
                      item#connect#activate ~callback:(fun () -> self#switch_mli_ml page);
                      let _ = GMenu.separator_item ~packing:menu#add () in
                      let item = GMenu.image_menu_item ~label:"Save As..." ~packing:menu#add () in
                      item#set_image (GMisc.image ~stock:`SAVE_AS ~icon_size:`MENU ())#coerce;
                      item#connect#activate ~callback:(fun () -> self#dialog_save_as page);
                      let item = GMenu.image_menu_item ~label:(sprintf "Rename \xC2\xAB%s\xC2\xBB" basename) ~packing:menu#add () in
                      item#connect#activate ~callback:(fun () -> self#dialog_rename page);
                      let item = GMenu.image_menu_item ~label:(sprintf "Delete \xC2\xAB%s\xC2\xBB" basename) ~packing:menu#add () in
                      item#set_image (GMisc.image ~stock:`DELETE ~icon_size:`MENU ())#coerce;
                      item#connect#activate ~callback:self#delete_current;
                      menu#popup ~time:(GdkEvent.Button.time ev) ~button:3;
                    end;
                    true
                  end else false
                end);
                (** Tab close button *)
                let align = GBin.alignment ~packing:ebox#add () in
                button_close#set_image image#coerce;
                button_close#connect#clicked ~callback:(fun () -> self#dialog_confirm_close page);
                let lab = GMisc.label ~markup:(Editor_page.markup_label filename) ~xalign:0.0 ~yalign:1.0 ~xpad:0 () in
                page#set_tab_widget (align, button_close, lab);
                (** Append tab *)
                let _ = notebook#append_page ~tab_label:ebox#coerce page#coerce in
                self#set_tab_pos ~page !Preferences.preferences.Preferences.pref_tab_pos;
                if active then begin
                  self#load_page page;
                  notebook#goto_page (notebook#page_num page#coerce);
                end;
                pages <- page :: pages;
                add_page#call page;
                page
              end
            end;
          end
        in
        Some page
      with e -> begin
        Dialog.display_exn self e;
        None
      end

    method populate_menu page =
      let gmenu = GMenu.menu () in
      List.iter (fun mi -> gmenu#prepend (mi ())) menu_items;
      gmenu#append (GMenu.separator_item ());
      let cut = GMenu.image_menu_item ~stock:`CUT ~packing:gmenu#append () in
      let copy = GMenu.image_menu_item ~stock:`COPY ~packing:gmenu#append () in
      let paste = GMenu.image_menu_item ~stock:`PASTE ~packing:gmenu#append () in
      let delete = GMenu.image_menu_item ~stock:`DELETE ~packing:gmenu#append () in
      gmenu#append (GMenu.separator_item ());
      let select_all = GMenu.image_menu_item ~stock:`SELECT_ALL ~packing:gmenu#append () in
      let callback ev =
        let clip = GData.clipboard Gdk.Atom.clipboard in
        cut#connect#activate ~callback:(fun () -> page#buffer#cut_clipboard clip);
        cut#misc#set_sensitive page#buffer#has_selection;
        copy#connect#activate ~callback:(fun () -> page#buffer#copy_clipboard clip);
        copy#misc#set_sensitive page#buffer#has_selection;
        paste#connect#activate ~callback:(fun () -> page#buffer#paste_clipboard clip);
        paste#misc#set_sensitive (clip#text <> None);
        delete#connect#activate ~callback:(fun () -> ignore (page#buffer#delete_selection ()));
        delete#misc#set_sensitive page#buffer#has_selection;
        select_all#connect#activate ~callback:page#buffer#select_all;
        gmenu#popup ~button:3 ~time:(GdkEvent.get_time ev);
        true in
      ignore(page#view#event#connect#button_press ~callback:begin fun ev ->
        if (GdkEvent.Button.button ev = 3 && GdkEvent.get_type ev = `BUTTON_PRESS) then begin
          let x = int_of_float (GdkEvent.Button.x ev) in
          if x < page#view#gutter.Gutter.size - page#view#gutter.Gutter.fold_size then begin
            Gaux.may bookmark_view ~f:begin fun bookmark_view ->
              let x, y = Gdk.Window.get_pointer_location (Gdk.Window.root_parent ()) in
              Gtk_util.window bookmark_view#coerce ~parent:self ~destroy_child:false ~x ~y ()
            end
          end else ignore ((callback ev));
          true
        end else false
      end);
      ignore (page#view#event#connect#key_press ~callback:begin fun ev ->
        if (GdkEvent.Key.keyval ev = GdkKeysyms._Menu) then (callback ev) else false
      end)

  method revert (page : Editor_page.page) = Gaux.may page#file ~f:begin fun file ->
    if page#buffer#modified then Dialog.confirm
      ~message:("File\n\""^page#get_filename^"\"\nmodified, revert?")
      ~f:((fun () -> page#revert()), ignore) page
  end

  method save_all () =
    List.iter (fun p -> if p#view#buffer#modified then self#save p) self#pages;

  method dialog_save_as page = self#dialog_save_as_rename ~action:`SAVE_AS page

  method dialog_rename page = self#dialog_save_as_rename ~action:`RENAME page

  method dialog_save_as_rename = Editor_dialog.save_as_rename ~editor:self

  method save (page : Editor_page.page) =
(*    try*)
      if !Preferences.preferences.Preferences.pref_editor_trim_lines
      then (page#buffer#trim_lines());
      page#save();
(*    with Not_found -> ()*)

  method dialog_confirm_close = Editor_dialog.confirm_close ~editor:self

  method dialog_save_modified = Editor_dialog.save_modified ~editor:self

  method delete_current () =
    self#with_current_page begin fun current ->
      let message = GWindow.message_dialog
        ~message:("Delete file\n"^current#get_filename^"?")
        ~message_type:`INFO
        ~position:`CENTER
        ~allow_grow:false
        ~destroy_with_parent:false
        ~modal:true
        ~buttons:GWindow.Buttons.yes_no () in
      Gaux.may (GWindow.toplevel self) ~f:(fun x -> message#set_transient_for x#as_window);
      (match message#run() with
        | `YES ->
          Gaux.may current#file ~f:(fun file -> Sys.remove file#path);
          self#close current;
          pages_cache <- List.filter (fun (_, p) -> p#misc#get_oid <> current#misc#get_oid) pages_cache;
          notebook#remove current#coerce;
        | _ -> ());
      message#destroy();
    end

  method close_all ?except () =
    let except = match except with
      | None -> fun p -> false
      | Some page -> fun p -> p = page
    in
    let modified, close = List.partition (fun p -> p#buffer#modified && (not (except p))) pages in
    closing <- true;
    List.iter (fun p -> if not (except p) then (self#close p)) close;
    closing <- false;
    if modified <> [] then begin
      let pages = List.rev_map (fun p -> true, p) modified in
      self#dialog_save_modified ~close:true ~callback:ignore pages
    end;

  method close page =
    Bookmark.write();
    if page#buffer#modified then (page#revert());
    page#buffer#set_modified false;
    pages <- List.filter ((<>) page) pages;
    remove_page#call page;
    (* Location history and autosave *)
    begin
      match page#file with
        | None -> ()
        | Some file ->
          pages_cache <- (file#path, page) :: pages_cache;
          page#misc#hide();
          (* File history *)
          File_history.add file_history file#path;
          (* Delete existing recovery copy *)
          page#set_changed_after_last_autosave false;
          page#buffer#set_changed_after_last_autocomp 0.0;
          Autosave.delete ~filename:file#path ();
    end;

  method redisplay_views () =
    self#with_current_page begin fun page ->
      Gmisclib.Idle.add page#redisplay;
      Gmisclib.Idle.add ~prio:400 (fun () -> page#compile_buffer ~commit:true ());
      List.iter (fun p -> if p != page then begin
        Gmisclib.Idle.add ~prio:300 p#redisplay;
        Gmisclib.Idle.add ~prio:400 (fun () -> p#compile_buffer ~commit:true ());
      end) pages;
    end;

  method i_search () =
    self#with_current_page begin fun page ->
      incremental_search#i_search ~view:(page#view :> Text.view) ~project:self#project
    end

  method location_history_is_empty () =
    (List.length (Location_history.get_history_backward self#location_history) = 0),
    (List.length (Location_history.get_history_forward self#location_history) = 0),
    ((Location_history.last_edit_location self#location_history) = None)

  method connect = new signals ~add_page ~switch_page ~remove_page ~changed ~modified_changed

  method private add_timeouts () =
    (** Auto-compilation *)
    ignore (GMain.Timeout.add ~ms:500 ~callback:begin fun () ->
      if project.Project.autocomp_enabled then begin
        try
          self#with_current_page begin fun page ->
            if page#buffer#changed_after_last_autocomp > 0.0 then begin
              if Unix.gettimeofday() -. page#buffer#changed_after_last_autocomp > project.Project.autocomp_delay (*/. 2.*)
              then (page#compile_buffer ~commit:false ())
            end
          end;
        with ex -> begin
          eprintf "%s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace ());
        end
      end;
      true
    end);
    (** Autosave *)
    if Oe_config.autosave_enabled then begin
      ignore (GMain.Timeout.add ~ms:Autosave.interval ~callback:begin fun () ->
        Prf.crono Prf.prf_autosave (List.iter begin fun page ->
          if page#changed_after_last_autosave then begin
            let filename = page#get_filename in
            let text = (page#buffer :> GText.buffer)#get_text () in
            Autosave.backup ~filename ~text;
            page#set_changed_after_last_autosave false;
          end
        end) pages;
        true
      end);
    end;
    (** Select current position in the outline and
        highlight matching delimiters *)
    let last_cursor_offset = ref 0 in
    ignore (GMain.Timeout.add ~ms:1500 ~callback:begin fun () ->
      self#with_current_page begin fun page ->
        let offset = (page#buffer#get_iter `INSERT)#offset in
        if not page#buffer#has_selection && offset <> !last_cursor_offset then begin
          page#view#matching_delim ();
          last_cursor_offset := (page#buffer#get_iter `INSERT)#offset
        end;
      end;
      true
    end);
    (*GMain.Timeout.add ~ms:1000 ~callback:begin fun () ->
      self#with_current_page begin fun page ->
        match page#buffer#matching_delims with
          | None ->
            page#buffer#set_matching_delims (Some (Prf.crono Prf.prf_delimiters_scan Delimiters.scan (page#buffer#get_text())));
            Printf.printf "Delimiters.scan\n%!" ;
          | _ -> ()
      end;
      true
    end;
    (**  *)*)


  initializer
    File_history.read file_history;
    (**  *)
    code_folding_enabled#set !Preferences.preferences.pref_code_folding_enabled;
    code_folding_enabled#connect#changed ~callback:begin fun enabled ->
      List.iter (fun p -> p#set_code_folding_enabled enabled) (pages @ (snd (List.split pages_cache)))
    end;
    (** When i_search finds text inside a fold, then expand the fold to show what is found. *)
    ignore (incremental_search#connect#found ~callback:begin fun view ->
      match self#get_page (Oe.Page_view view) with
        | Some page -> page#ocaml_view#code_folding#expand_current ()
        | _ -> ()
    end);
    (**  *)
    show_global_gutter#connect#changed ~callback:begin fun enabled ->
      List.iter (fun p -> if enabled then p#global_gutter#misc#show()
        else p#global_gutter#misc#hide()) (pages @ (snd (List.split pages_cache)))
    end;
    show_global_gutter#set !Preferences.preferences.pref_show_global_gutter;
    (**  *)
    self#add_timeouts();
    ignore (Liim.start liim_delim);
    ignore (Liim.start liim_fast);
    (** Switch page: update the statusbar and remove annot tag *)
    ignore (notebook#connect#after#switch_page ~callback:begin fun num ->
      (* Clean up type annotation tag and error indications *)
      List.iter (fun page -> page#annot_type#remove_tag ()) pages;
      (* Current page *)
      self#with_current_page begin fun page ->
        if not page#load_complete then (self#load_page page);
        page#update_statusbar();
        page#set_code_folding_enabled code_folding_enabled#get; (* calls scan_folding_points, if enabled *)
        page#view#paint_current_line_background (page#buffer#get_iter `INSERT);
        match page#outline with
          | Some outline when self#show_outline && outline#get_oid <> hpaned#child1#get_oid ->
            self#pack_outline outline#coerce
          | _ -> page#compile_buffer ~commit:false ()
      end;
      switch_page#call notebook#current_page;
    end);
    (** Record last active page *)
    ignore (notebook#connect#switch_page ~callback:begin fun num ->
      if notebook#current_page >= 0 then (last_active_page <- Some (notebook#get_nth_page notebook#current_page));
    end);
    (** Remove Page: editor goes to the last active page *)
    ignore (self#connect#remove_page ~callback:begin fun removed ->
      match self#get_page Oe.Page_current with
        | Some cur when cur#get_oid = removed#get_oid ->
          Gaux.may last_active_page ~f:begin fun last ->
            notebook#goto_page (notebook#page_num last)
          end
        | _ -> ()
    end);
    (* Replace marks with offsets in location history *)
    ignore (self#connect#remove_page ~callback:begin fun page ->
      Location_history.iter location_history ~f:begin function
        | loc when loc.Location_history.filename = page#get_filename ->
          if not page#buffer#modified (* i.e. saved *) then begin
            match loc.Location_history.mark with
              | Some mark when (not (GtkText.Mark.get_deleted mark)) ->
                let iter = page#buffer#get_iter_at_mark (`MARK mark) in
                loc.Location_history.offset <- iter#offset;
                loc.Location_history.mark <- None;
              | Some mark -> loc.Location_history.mark <- None;
              | _ -> ()
          end else begin
            (* If the buffer is not saved, location is unmeaningful; it only
               records the file name. *)
            loc.Location_history.mark <- None;
            loc.Location_history.offset <- 0;
          end
        | _ -> ()
      end
    end);
    bookmark_view <- Some (new Bookmark_view.widget ~editor:self ())
end

(** Signals *)
and switch_page () = object (self) inherit [int] signal () as super end
and remove_page () = object (self) inherit [Editor_page.page] signal () end
and modified_changed () = object (self) inherit [unit] signal () end
and changed () = object (self) inherit [unit] signal () end
and add_page () = object (self) inherit [Editor_page.page] signal () as super end

and signals ~add_page ~switch_page ~remove_page ~changed ~modified_changed =
object (self)
  inherit ml_signals [switch_page#disconnect;
    remove_page#disconnect; modified_changed#disconnect;
    add_page#disconnect; changed#disconnect]
  method switch_page = switch_page#connect ~after
  method remove_page = remove_page#connect ~after
  method modified_changed = modified_changed#connect ~after
  method changed = changed#connect ~after
  method add_page = add_page#connect ~after
end





