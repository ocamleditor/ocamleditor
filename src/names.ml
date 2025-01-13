open Printf
open Merlin_t
open Utils
open Fuzzy_search

module Log = Common.Log.Make(struct let prefix = "NAMEDB" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `DEBUG

type mod_entry = {
  filename : string;
  modname : string;
  mutable timestamp : float;
  mutable values : Merlin_t.entry list;
}

type db = {
  mutable table : mod_entry list;
  mutable timestamp : float;
}

module Lock = struct
  let namedb = Mutex.create()
  let [@inline] mutex mx f x =
    Mutex.lock mx;
    try
      let res = f x in
      Mutex.unlock mx;
      res
    with ex ->
      Mutex.unlock mx;
      raise ex
end

(** [!!mutex f x] applies [f] to [x] inside a critical section locked by
    [mutex] and returns its result. *)
let (!!) mx f x = Lock.mutex mx f x

(*module Project_watcher = struct

  let create (project : Prj.t) =
    let inotify = Inotify.create() in
    let path = Project.path_src project in
    let watch = Inotify.add_watch inotify path [Inotify.S_Create; Inotify.S_Delete; Inotify.S_Modify] in
    ()
  end*)

let count db = db.table |> List.fold_left (fun sum x -> sum + List.length x.values) 0

let database : db option ref = ref None

let get_project_source_filenames project =
  File_util.readtree (Project.path_src project)
  |> List.map (fun dir ->
      File_util.ls ~dir ~pattern:"*.ml" |> List.map (Filename.concat dir))
  |> List.concat

let filter pattern db =
  let compare = Utils.Memo.fast ~f:(fun (a, b) -> FuzzyLetters.compare `Greedy a b) in
  !!Lock.namedb begin fun () ->
    db.table
    |> List.map (fun entry ->
        entry.values
        |> List.filter_map begin fun val_entry ->
          let score, paths = compare (pattern, val_entry.name) in
          if score > 0. then Some (score, val_entry) else None
        end)
    |> List.concat
    |> List.sort (fun (a, _) (b, _) -> Stdlib.compare b a)
  end ()

let rec add_outline db entry modname outline =
  let temp = ref [ { kind = "Module"; name = modname; desc = ""; info = "" } ] in
  outline
  |> List.iter begin fun item ->
    let name = modname ^ "." ^ item.ol_name in
    if item.ol_kind = "Module" then begin
      let value = { kind = item.ol_kind; name = item.ol_name; desc = ""; info = "" } in
      temp := value :: !temp;
      add_outline db entry name item.ol_children;
      add_outline db entry item.ol_name item.ol_children;
    end else begin
      let value = { kind = item.ol_kind; name; desc = ""; info = "" } in
      temp := value :: !temp
    end
  end;
  !!Lock.namedb (fun () -> entry.values <- List.rev_append !temp entry.values) ()

let rec add_compl db modname compl count_lib_values =
  let entry = { filename=""; modname; timestamp=0.0; values=[] } in
  entry.values <- { name = modname; kind = "Module"; desc = ""; info = "" } :: entry.values;
  compl
  |> List.iter (fun (item : Merlin_j.entry) ->
      if item.kind = "Module" then begin
        Thread.delay 20.;
        complete_prefix db (sprintf "%s.%s" modname item.name) count_lib_values
      end;
      entry.values <- { item with name = modname ^ "." ^ item.name } :: entry.values);
  entry

and complete_prefix db modname count_lib_values =
  Merlin.complete_prefix ~position:(1,1) ~prefix:(modname ^ ".") ~filename:"dummy" ~source_code:""
    begin fun compl ->
      let mod_entry = add_compl db modname compl.entries count_lib_values in
      !!Lock.namedb begin fun () ->
        mod_entry.timestamp <- Unix.gettimeofday();
        db.table <- mod_entry :: db.table;
        count_lib_values := !count_lib_values + List.length mod_entry.values
      end ()
    end

let update ?(is_init=false) db filename =
  if is_init then Thread.delay 0.075
  else
    !!Lock.namedb (fun () ->
        db.table <- db.table |> List.filter (fun me -> me.filename <> filename)) ();
  if Sys.file_exists filename then begin
    let modname =
      filename |> Filename.basename |> Filename.remove_extension |> String.capitalize_ascii in
    let mod_entry = { filename; modname; timestamp=Unix.gettimeofday(); values=[] } in
    !!Lock.namedb (fun () -> db.table <- mod_entry :: db.table) ();
    Merlin.outline
      ~filename
      ~source_code:(File_util.read filename |> Buffer.contents)
      begin fun outline ->
        add_outline db mod_entry modname outline;
        !!Lock.namedb (fun () -> mod_entry.timestamp <- Unix.gettimeofday()) ();
      end;
  end

let init ?(cont=ignore) project =
  let db = { table=[]; timestamp=Unix.gettimeofday() } in
  database := Some db;
  let count_lib_values = ref 0 in
  get_project_source_filenames project |> List.iter (update ~is_init:true db);
  Merlin.list_modules ()
  |> Async.map ~name:__FUNCTION__ begin function
  | Merlin.Ok modules ->
      "Str" :: "Unix" :: "Thread" :: modules
      |> List.iter begin fun modname ->
        match List.find_opt (fun me -> me.modname = modname) db.table with
        | None -> complete_prefix db modname count_lib_values
        | _ -> ()
      end
  | Merlin.Failure msg | Merlin.Error msg ->
      Log.println `ERROR "%s" msg
  end
  |> Async.start;
  cont db

let update_all ?(cont=ignore) project =
  match !database with
  | Some db ->
      Thread.create begin fun () ->
        get_project_source_filenames project
        |> List.iter begin fun filename ->
          match db.table |> List.find_opt (fun x -> x.filename = filename) with
          | Some dbf when (Unix.stat filename).Unix.st_mtime <= dbf.timestamp -> ()
          | _ -> update db filename
        end;
        cont db
      end () |> ignore;
  | _ ->
      let message = "Loading completion..." in
      Activity.add Activity.Other message;
      project |> init ~cont:begin fun db ->
        Activity.remove message;
        cont db
      end;
      Activity.wrap ~delay:750 Activity.Other ignore ()

module Cache = struct

  let create_filename ~project =
    let cache_dir = Project.path_cache project in
    Utils.mkdir_p cache_dir;
    cache_dir // "names";;

  let reset ~project =
    let name = (sprintf "Loading names (%s)..." project.Prj.name) in
    Log.println `DEBUG "%s -- %s" __FUNCTION__ name;
    Thread.create begin fun () ->
      GtkThread.async (Activity.add Activity.Symbol) name;
      init ~cont:begin fun _ ->
        GtkThread.async Activity.remove name;
        Log.println `DEBUG "initialized %s" name;
      end project
    end () |> ignore;;

  let save ~project =
    match !database with
    | Some table ->
        !!Lock.namedb begin fun () ->
          let filename = create_filename ~project in
          let chan = open_out_bin filename in
          let finally () = close_out chan in
          try
            let cache = Oe.Dump (Oe.magic, table) in
            output_value chan cache;
            Log.println `DEBUG "Saved %d entries to %s" (count table) filename;
            finally();
          with ex -> begin
              finally();
              raise ex
            end
        end ();
    | _ -> ();;

  let load ~project =
    let filename = create_filename ~project in
    if Sys.file_exists filename then begin
      !!Lock.namedb begin fun () ->
        let chan = open_in_bin filename in
        let finally () = close_in chan in
        try
          let (db : db) = Oe.open_dump (input_value chan) in
          database := Some db;
          Log.println `DEBUG "Loaded %d entries from %s" (count db) filename;
          finally();
        with Oe.Bad_magic_number -> begin
            finally();
            reset ~project
          end | ex -> begin
            finally();
            raise ex
          end
      end ()
    end else (reset ~project);;

end
