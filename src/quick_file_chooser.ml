(*

  OCamlEditor
  Copyright (C) 2010-2013 Francesco Tovagliari

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

type t = {
  source                : [ `path of string list * string list (* roots x filenames *)
                          | `filelist of string list (* filenames *)];
  model                 : GTree.list_store;
  index_0               : (char, Gtk.tree_path) Hashtbl.t;   (* basename.[0] x tree_path *)
  index_1               : (char, Gtk.tree_path) Hashtbl.t;   (* basename.[1] x tree_path *)
  index_c               : (char, Gtk.tree_path) Hashtbl.t;   (* basename.[any] x tree_path *)
  index_cc              : (string, Gtk.tree_path) Hashtbl.t;
  mutable index_icons   : Gtk.tree_path list;
  mutable index_removed : Gtk.tree_path list;
  filenames             : (string, Gtk.tree_path) Hashtbl.t; (* filename x true *)
  rtimes                : (string, float) Hashtbl.t;         (* dirname x last read *)
  mutable length        : int;
}

let limit = 100_000

let models : (string * t) list ref = ref []

let re_dot = !~~ ".";;
let re_question = !~~ "?";;
let re_asterisk = !~~ "*";;
let re_wildcards = !~ "[?*]";;

let crono_sum time f x =
  let finally t =
    time := !time +. (Unix.gettimeofday() -. t);
  in
  let t = Unix.gettimeofday() in
  let result = try f x with e -> begin
    finally t;
    raise e
  end in
  finally t;
  result;;

let cols         = new GTree.column_list
let col_pixbuf   = cols#add (Gobject.Data.gobject_option : (GdkPixbuf.pixbuf option) Gobject.data_conv)
let col_name     = cols#add Gobject.Data.string
let col_path     = cols#add Gobject.Data.string
let col_visible  = cols#add Gobject.Data.boolean

(** create_model *)
let create_model ~source ~name =
  let model       = GTree.list_store cols in
  let model       = {
    source        = source;
    model         = model;
    index_0       = Hashtbl.create 17;
    index_1       = Hashtbl.create 17;
    index_c       = Hashtbl.create 17;
    index_cc      = Hashtbl.create 17;
    index_icons   = [];
    index_removed = [];
    filenames     = Hashtbl.create 17;
    rtimes        = Hashtbl.create 7;
    length        = 0;
  } in
  begin
    match source with
      | `path (roots, _) -> models := (name, model) :: !models;
      | _ -> ()
  end;
  model;;

let char_contained str =
  let res = ref [] in
  String.iter (fun c -> if not (List.mem c !res) then res := c :: !res)
    (String.sub str 1 (String.length str - 2));
  !res;;

let regexp_of_pattern pattern =
  let pattern = (Str.global_replace re_dot "\\." pattern) in
  let pattern = (Str.global_replace re_question "." pattern) in
  let pattern = (Str.global_replace re_asterisk "[-+ 'a-zA-Z!^%&$()@#;.,_0-9]*" pattern) in
  Miscellanea.regexp_case_fold pattern;;

(** widget *)
class widget ~source ~name ?filter ?packing () =
  let ebox              = GBin.event_box ?packing () in
  let _                 = ebox#misc#set_property "visible-window" (`BOOL false) in
  let vbox              = GPack.vbox ~spacing:5 ~packing:ebox#add () in
  let sbox              = GPack.vbox ~spacing:2 ~packing:vbox#pack () in
  let box               = GPack.hbox ~spacing:5 ~packing:sbox#pack () in
  let _                 = GMisc.label ~markup:"Search for <small>(\"<tt>?</tt>\" matches any single character, \"<tt>*</tt>\" matches any string)</small>:" ~xalign:0.0 ~yalign:1.0 ~packing:box#add () in
  let label_count       = GMisc.label ~xalign:1.0 ~yalign:1.0 ~height:16 ~packing:box#pack () in
  let icon_progress     = GMisc.image ~width:16 ~file:(App_config.application_icons // "spinner_16.gif") ~packing:box#pack () in
  let entry             = GEdit.entry ~packing:sbox#pack () in
  let model, filelist =
    match source with
      | `path (roots, filelist) ->
        (try List.assoc name !models with Not_found -> create_model ~source ~name), filelist
      | `filelist filelist -> (create_model ~source ~name), filelist
  in
  let model_filter      = GTree.model_filter model.model in
  let _                 = model_filter#set_visible_column col_visible in
  (** View *)
  let renderer          = GTree.cell_renderer_text [] in
  let renderer_basename = GTree.cell_renderer_text [] in
  let renderer_pixbuf   = GTree.cell_renderer_pixbuf [] in
  let _                 = renderer_basename#set_properties [] in
  let vc_pixbuf         = GTree.view_column ~title:"" ~renderer:(renderer_pixbuf, ["pixbuf", col_pixbuf]) () in
  let vc_name           = GTree.view_column ~title:"File" ~renderer:(renderer_basename, ["text", col_name]) () in
  let vc_path           = GTree.view_column ~title:"Path" ~renderer:(renderer, ["text", col_path]) () in
  let sw                = GBin.scrolled_window ~shadow_type:`IN ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:vbox#add () in
  let view              = GTree.view ~headers_visible:false ~reorderable:true ~width:130
    ~enable_search:false ~packing:sw#add () in
  let _                 = view#append_column vc_pixbuf in
  let _                 = view#append_column vc_name in
  let _                 = view#append_column vc_path in
  let _                 = view#selection#set_mode `MULTIPLE in
  let _                 = view#set_model (Some model_filter#coerce) in
object (self)
  inherit GObj.widget ebox#as_widget
  val is_filelist = match source with `filelist _ -> true | _ -> false
  val mutable table_visible = []
  val mutable table_results = []
  val mutable choose_func = fun ~filename ~has_cursor -> `ignore
  val mutable timeout_id = None
  val mutable cache_results = []
  val mutable last_pattern = ""
  val mutable queue = Queue.create ()

  initializer self#init()

  method source = source
  method model = model.model
  method view = view
  method renderer = renderer_basename
  method select_path path = view#selection#select_path (model_filter#convert_child_path_to_path path);
  method set_cursor path = view#set_cursor (model_filter#convert_child_path_to_path path) vc_name;

  method get_paths_with_icon () = model.index_icons

  method get_path ~filename =
    try Some (Hashtbl.find model.filenames filename) with Not_found -> None

  method get_basename ~path =
    let row = model.model#get_iter path in
    row, model.model#get ~row ~column:col_name

  method get_dirname ~row = model.model#get ~row ~column:col_path

  method set_default_choose_func f = choose_func <- f

  method set_cell_data_func = vc_name#set_cell_data_func renderer_basename

  method set_icon ~row pixbuf =
    model.model#set ~row ~column:col_pixbuf pixbuf;
    let path = model.model#get_path row in
    match pixbuf with
      | None -> model.index_icons <- List.filter (fun x -> x <> path) model.index_icons
      | _ -> model.index_icons <- path :: model.index_icons

  method reset_icons () =
    List.iter begin fun path ->
      let row = model.model#get_iter path in
      let basename = model.model#get ~row ~column:col_name in
      model.model#set ~row ~column:col_pixbuf None
    end model.index_icons;
    model.index_icons <- []

  method update_model ?blocking () =
    let scan () =
      self#scan ?blocking ();
      view#selection#select_path (GTree.Path.create [0]);
    in
    match blocking with
      | Some true -> (*crono ~label:"scan"*) scan ();
      | _ -> Gmisclib.Idle.add ~prio:300 scan

  method activate :
      ?f:(filename:string -> has_cursor:bool -> [`clear | `set of GdkPixbuf.pixbuf | `ignore]) ->
        unit -> unit = fun ?f () ->
    let path_cursor, _ = view#get_cursor () in
    let has_cursor =
      match path_cursor with None -> (fun _ -> false) | Some p ->
        let p, _ = self#convert_path_to_child_path p in
        let p = GTree.Path.to_string p in (fun x -> p = GTree.Path.to_string x)
    in
    let f = match f with Some f -> f | _ -> choose_func in
    List.iter begin fun path ->
      let path, row = self#convert_path_to_child_path path in
      let dirname = model.model#get ~row ~column:col_path in
      let basename = model.model#get ~row ~column:col_name in
      let pixbuf = f ~filename:(dirname // basename) ~has_cursor:(has_cursor path) in
      match pixbuf with
        | `ignore -> ()
        | `clear ->
          model.model#set ~row ~column:col_pixbuf None;
          model.index_icons <- List.filter (fun x -> x <> path) model.index_icons
        | `set pixbuf ->
          model.model#set ~row ~column:col_pixbuf (Some pixbuf);
          model.index_icons <- path :: model.index_icons
    end view#selection#get_selected_rows;

  method finalize () =
    Gaux.may timeout_id ~f:GMain.Timeout.remove;
    Gmisclib.Idle.add self#clear_model_filter;
    vc_name#unset_cell_data_func renderer_basename

  method private convert_path_to_child_path path =
    let current_model_oid = view#model#misc#get_oid in
    let model_filter_oid = model_filter#misc#get_oid in
    if current_model_oid = model_filter_oid then
      let row = model_filter#get_iter path in
      let row = model_filter#convert_iter_to_child_iter row in
      let path = model_filter#convert_path_to_child_path path in
      path, row
    else
      let row = model.model#get_iter path in
      path, row

  method private convert_child_path_to_path path =
    let current_model_oid = view#model#misc#get_oid in
    let model_filter_oid = model_filter#misc#get_oid in
    if current_model_oid = model_filter_oid then
      let row = model.model#get_iter path in
      path, row
    else
      let row = model_filter#get_iter path in
      let row = model_filter#convert_child_iter_to_iter row in
      let path = model_filter#convert_child_path_to_path path in
      path, row

  method private visible_func ~row pattern =
    entry#text <> "" &&
      let basename = model.model#get ~row ~column:col_name in
      Str.string_match pattern basename 0

  method private apply_pattern () =
    begin
      let len = String.length entry#text in
      if len = 0 then begin
        if is_filelist then (self#display_all()) else (self#clear_model_filter ());
      end else if entry#text = "*" then begin
        self#display_all ();
      end else begin
       (* if self#is_refinement() then (self#search_by_pattern ())
        else *)begin
          let is_dirty = self#search_by_index () in
          if is_dirty then self#search_by_pattern ();
        end;
        self#display table_results;
      end;
      if table_results <> [] then cache_results <- (entry#text, table_results) :: cache_results;
    end;
    if not (List.mem entry#text ["*"]) then last_pattern <- entry#text

  method private analyze_pattern parts =
    List.fold_left begin fun acc text ->
      match String.length text with
        | 0 -> acc
        | 1 ->
          let results = Hashtbl.find_all model.index_c (String.unsafe_get text 0) in
          (text, List.length results, results) :: acc
        | 2 ->
          let results = Hashtbl.find_all model.index_cc text in
          (text, List.length results, results) :: acc
        | len ->
          let ccs = ref [] in
          let i = ref 0 in
          while !i < len - 1 do
            ccs := (String.sub text !i 2) :: !ccs;
            incr i;
          done;
          (self#analyze_pattern !ccs) @ acc
    end [] parts;

  method private intersect results =
    let results = List.sort (fun (_, l1, _) (_, l2, _) -> Pervasives.compare l1 l2) results in
    (*Printf.printf "results=[%s]\n%!"
      (String.concat "; " (List.map (fun (t, _, r) -> sprintf "%s=%d" t (List.length r)) results));*)
    match results with
      | (_, _, i1) :: [] -> i1
      | (k1, _, i1) :: (k2, _, i2) :: _ when k1 <> k2 ->
        (* The smaller data set is filtered over the bigger.
           "index_cc" can contain multiple occurrences of the same tree path
           (like "ab" in "abxaby" ) hence filtering "i1" over "i2" can give a
           different result from filtering "i2" over "i1".
           However, when the first character of the pattern is not a wildcard and
           the pattern length is greater than 1, "i1" is from "index_0"
           and connot contain duplicates. *)
        (*crono ~label:"intersect"*) (List.filter (fun x -> List.mem x i2)) i1; (* OK *)
      | (_, _, i1) :: (_, _, i2) :: _ -> i1
      | [] -> [] (*assert false*)

  method private search_by_index () =
    (*crono ~label:"search_by_index" begin fun () ->*)
      let len = String.length entry#text in
      let pattern = String.lowercase entry#text in
      let c0 = String.unsafe_get pattern 0 in
      let dirty, results =
        match c0 with
          | '?' when len = 2 ->
            let c = String.unsafe_get pattern 1 in
            let results = Hashtbl.find_all model.index_1 c in
            false, [sprintf "?%c" c, List.length results, results]
          | '?' when len = 3 ->
            let cc = String.sub pattern 1 2 in
            let results = Hashtbl.find_all model.index_cc cc in
            true, [sprintf "%s" cc, List.length results, results]
          | c0 when len = 1 && c0 <> '*' && c0 <> '?' -> (* a *)
            false,
              let i0 = Hashtbl.find_all model.index_0 c0 in
              [sprintf "(0)%c" c0, List.length i0, i0]
          | c0 when len = 2 && c0 <> '*' && c0 <> '?' &&
              (String.unsafe_get pattern 1 = '*' || String.unsafe_get pattern 1 = '?') -> (* a? *)
            false,
              let i0 = Hashtbl.find_all model.index_0 c0 in
              [sprintf "(0)%c" c0, List.length i0, i0]
          | c0 when len = 2 && c0 <> '*' && c0 <> '?' &&
              String.unsafe_get pattern 1 <> '*' && String.unsafe_get pattern 1 <> '?' -> (* ab *)
            false,
              let c1 = String.unsafe_get pattern 1 in
              let i0 = Hashtbl.find_all model.index_0 c0 in
              let i1 = Hashtbl.find_all model.index_1 c1 in
              [sprintf "(0)%c" c0, List.length i0, i0; sprintf "(1)%c" c1, List.length i1, i1]
          | c0 when len = 3 && c0 <> '*' && c0 <> '?' &&
              String.unsafe_get pattern 1 <> '*' && String.unsafe_get pattern 1 <> '?' &&
              (String.unsafe_get pattern 2 = '*' || String.unsafe_get pattern 2 = '?') -> (* ab? *)
            false,
              let c1 = String.unsafe_get pattern 1 in
              let i0 = Hashtbl.find_all model.index_0 c0 in
              let i1 = Hashtbl.find_all model.index_1 c1 in
              [sprintf "(0)%c" c0, List.length i0, i0; sprintf "(1)%c" c1, List.length i1, i1]
          | _ ->
            let parts = Str.split re_wildcards pattern in
            let results = (*crono ~label:"analyze_pattern"*) self#analyze_pattern parts in
            (List.length results > 1), results
      in
      let results = self#intersect results in
      let results = List.filter (fun p -> not (List.mem p model.index_removed)) results in
      table_results <- results;
      dirty
   (* end ()*)

  method private search_by_pattern () =
    (*crono ~label:"search_by_pattern" begin fun () ->
      Printf.printf "refine: %d->%!" (List.length table_results);*)
      let pattern = regexp_of_pattern entry#text in
      table_results <- List.fold_left begin fun acc path ->
        let row = model.model#get_iter path in
        let is_visible = self#visible_func ~row pattern in
        if is_visible then path :: acc else acc
      end [] table_results;
      (*Printf.printf "%d\n%!" (List.length table_results);
    end ()*)

  method display paths =
    view#set_model None;
    let unchanged, hide = List.partition (fun x -> List.mem x paths) table_visible in
    List.iter begin fun path ->
      let row = model.model#get_iter path in
      model.model#set ~row ~column:col_visible false;
    end hide;
    table_visible <- unchanged;
    let count = ref (List.length table_visible) in
    let tmp = ref [] in
    begin
      try
        List.iter begin fun path ->
          let add = not (List.mem path table_visible) in
          if add then begin
            tmp := path :: !tmp;
            let row = model.model#get_iter path in
            model.model#set ~row ~column:col_visible true;
            incr count;
            if !count >= limit then (raise Exit)
          end;
        end paths;
      with Exit -> ()
    end;
    table_visible <- List.rev_append table_visible !tmp;
    model_filter#refilter ();
    view#set_model (Some model_filter#coerce);
    Gmisclib.Idle.add (fun () -> view#scroll_to_point 0 0);
    self#display_summary ();
    Gmisclib.Idle.add begin fun () ->
      match model_filter#get_iter_first with
        | Some row ->
          let path = model_filter#get_path row in
          view#selection#select_path path;
          view#set_cursor path vc_name;
        | _ -> ()
    end

  method display_all () =
    self#clear_model_filter();
    if model.length <= limit then begin
      view#set_model (Some model.model#coerce);
      self#display_summary();
    end;

  method private display_summary () =
    kprintf label_count#set_text "%d of %d" (List.length table_visible) model.length;

  method private clear_model_filter () =
    view#set_model None;
    List.iter begin fun path ->
      model.model#set ~row:(model.model#get_iter path) ~column:col_visible false;
    end table_visible;
    table_visible <- [];
    table_results <- [];
    self#display_summary ()

  method private append pattern filename =
    if not (Hashtbl.mem model.filenames filename) then begin
      let dirname = Filename.dirname filename in
      let basename = Filename.basename filename in
      let row = model.model#append() in
      let path = model.model#get_path row in
      model.length <- model.length + 1;
      model.model#set ~row ~column:col_name basename;
      model.model#set ~row ~column:col_path dirname;
      (*  *)
      let is_visible = self#visible_func ~row pattern in
      model.model#set ~row ~column:col_visible is_visible;
      if is_visible then (table_visible <- path :: table_visible);
      (* Create indexes *)
      let low_basename = String.lowercase basename in
      let len = String.length low_basename in
      Hashtbl.add model.index_0 low_basename.[0] path;
      if len > 1 then Hashtbl.add model.index_1 low_basename.[1] path;
      List.iter (fun c -> Hashtbl.add model.index_c c path) (char_contained low_basename);
      let i = ref 0 in
      while !i < len - 1 do
        let cc = String.sub low_basename !i 2 in
        Hashtbl.add model.index_cc cc path;
        incr i;
      done;
      (* An index entry for the last single char. *)
      let cc = String.sub low_basename (len - 1) 1 in
      Hashtbl.add model.index_cc cc path;
      (*  *)
      Hashtbl.add model.filenames filename path;
      self#display_summary ()
    end;

  method private scan ?(blocking=false) () =
    let time = ref 0.0 in
    let queue = Queue.create () in
    let filter_dir d = not (List.mem d [".tmp"; "bak"]) in
    let do_search dirname =
      if filter_dir (Filename.basename dirname) then begin
        let filenames = File_util.readdirs ~recursive:false filter dirname in
        List.iter (fun x ->  Queue.add x queue) filenames;
        Hashtbl.replace model.rtimes dirname (Unix.gettimeofday());
      end
    in
    let scan_tree roots =
      match !roots with
        | root :: tl ->
          roots := tl;
          let dirs = File_util.readtree root in
          List.iter begin fun dirname ->
            try
              let stat = Unix.stat dirname in
              let rtime = Hashtbl.find model.rtimes dirname in
              if stat.Unix.st_mtime > rtime then do_search dirname;
            with Not_found -> (do_search dirname)
          end dirs;
          true
        | [] -> false
    in
    let append roots =
      let pattern = regexp_of_pattern entry#text in
      (*try*)
        begin
          let i = ref 10 in
          while !i > 0 && Queue.length queue > 0 do
            self#append pattern (Queue.take queue);
            decr i;
          done;
          if !roots = [] && Queue.is_empty queue then begin
            self#prune ();
            table_results <- table_visible;
            if is_filelist then (self#apply_pattern());
            self#display_summary ();
            icon_progress#set_pixbuf Icons.empty_14;
            false
          end else true
        end;
      (*with ex ->
        Printf.eprintf "File \"dialog_find_file.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
        false*)
    in
    let filelist =
      match model.source with
        | `path (roots, filelist) ->
          let roots = ref roots in
          if blocking then begin
            while scan_tree roots do () done;
            while append roots do () done;
          end else begin
            let id = GMain.Timeout.add ~ms:200 ~callback:(fun () -> scan_tree roots) in
            ignore (self#misc#connect#destroy ~callback:(fun () -> GMain.Timeout.remove id));
            let id = GMain.Timeout.add ~ms:15 ~callback:(fun () -> append roots) in
            ignore (self#misc#connect#destroy ~callback:(fun () -> GMain.Timeout.remove id));
          end;
          filelist
        | `filelist x -> x
    in
    let pattern = regexp_of_pattern entry#text in
    List.iter (self#append pattern) filelist;
    (*self#prune()*)

  method private prune () =
    (*crono ~label:"prune" begin fun () ->*)
      let count = ref 0 in
      Hashtbl.iter begin fun filename path ->
        if not (Sys.file_exists filename) then begin
          model.index_removed <- path :: model.index_removed;
          Hashtbl.remove model.filenames filename;
          incr count;
          (*table_visible <- List.filter (fun x -> x <> path) table_visible;
              table_results <- List.filter (fun x -> x <> path) table_results;*)
        end;
      end model.filenames;
      (* DO NOT REMOVE ROWS FROM THE MODEL TO NOT ALTER PATHS IN INDEXES. *)
      (*List.iter (fun (filename, iter) -> ignore (model.model#remove iter)) !removing;*)
      model.length <- model.length - !count;
      self#display_summary ()
    (*end ()*)

  method private is_refinement () =
    let len = String.length entry#text in
    let len_last = String.length last_pattern in
    len_last + 1 = len &&
      String.sub entry#text 0 (len - 1) = last_pattern

  method private init () =
    ignore (view#misc#connect#destroy ~callback:self#finalize);
    (** Entry key press *)
    ignore (entry#connect#changed ~callback:begin fun () ->
      Gmisclib.Idle.add ~prio:200 self#apply_pattern;
    end);
    ignore (ebox#event#connect#key_press ~callback:begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
      if key = GdkKeysyms._Up || key = GdkKeysyms._Down then begin
        let clean = view#selection#get_selected_rows = [] in
        if clean then begin
          (try view#selection#select_path (GTree.Path.create [0]) with _ -> ());
        end else begin
          match view#selection#get_selected_rows with
            | path :: _ ->
              let path_0 = GTree.Path.copy path in
              if key = GdkKeysyms._Up && GTree.Path.prev path then ()
              else if key = GdkKeysyms._Down then (GTree.Path.next path);
              view#set_cursor path vc_name;
              if GdkEvent.Key.state ev = [`SHIFT] then (view#selection#select_range path_0 path)
              else (view#selection#select_path path);
            | [] -> ()
        end;
        view#misc#grab_focus();
        not clean
      end else false
    end);
    (** View key press *)
    ignore (view#event#connect#key_release ~callback:begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
      if key = GdkKeysyms._BackSpace then begin
        (try entry#set_text (String.sub entry#text 0 (entry#text_length - 1)) with _ -> ());
        entry#misc#grab_focus();
        entry#set_position entry#text_length;
        true
      end else if key = GdkKeysyms._Left then begin
        entry#misc#grab_focus();
        entry#set_position (entry#text_length - 1);
        true
      end else if key = GdkKeysyms._Right then begin
        entry#misc#grab_focus();
        entry#set_position entry#text_length;
        true
      end else if key = GdkKeysyms._F3 then begin
        entry#misc#grab_focus(); true
      end else begin
        let str = GdkEvent.Key.string ev in
        if String.length str > 0 && Glib.Unichar.isprint (Glib.Utf8.first_char str) then
          entry#set_text (entry#text ^ str);
        false
      end
    end);
    (**  *)
    entry#misc#grab_focus();
    if is_filelist then (self#display_all())
end


let init ~roots ~filter =
  let widget = new widget ~source:(`path (roots, [])) ~name:"" ~filter () in
  widget#update_model ~blocking:true ();;

let add_roots ~roots ~filter =
  let name = "" in
  match List_opt.assoc name !models with
    | Some model ->
      models := List.remove_assoc name !models;
      begin
        match model.source with
          | `path (rr, filelist) ->
            let roots = rr @ roots in
            init ~roots ~filter
          | _ -> assert false
      end
    | _ -> init ~roots ~filter



