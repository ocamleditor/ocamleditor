(*

  OCamlEditor
  Copyright (C) 2010-2014 Francesco Tovagliari

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


open Types
open Outcometree
open Oe
open Miscellanea
open Printf

let get_parent_path symbol =
  match List.rev symbol.sy_id with
    | _ :: tl -> List.rev tl
    | _ -> []

let get_name symbol =
  match List.rev symbol.sy_id with x :: _ -> x | _ -> assert false

let get_module_name symbol =
  match symbol.sy_id with x :: _ -> x | _ -> assert false

let is_method symbol =
  match [@warning "-4"] symbol.sy_kind with
    | Pmethod | Pmethod_private | Pmethod_virtual | Pmethod_private_virtual -> true
    | _ -> false

let concat_value_path symbol =
  (*if is_method symbol then begin
    let rec f acc = function
      | [] -> acc
      | a :: b :: [] -> acc ^ "." ^ a ^ "#" ^ b
      | a :: tl -> f (acc ^ a) tl
    in f "" symbol.sy_id
  end else*) (String.concat "." symbol.sy_id)

let split_value_path id = Longident.flatten (Longident.parse id)

let string_of_id = String.concat "."


module Modules = struct
  type t = {
    mo_name     : string;
    mo_filename : string;
    mo_descr    : string;
  }

  let read' (filter, path) =
    let len = String.length filter in
    let filter = String.lowercase_ascii filter in
    (* Read .cmi files from path *)
    let filenames = Array.to_list (try Sys.readdir path with (Sys_error _) -> [||]) in
    let module_files =
      List.filter begin fun x ->
        (len = 0 || String.length x >= len) &&
        (String.lowercase_ascii (Str.first_chars x len)) = filter (* TODO: improve *)
        && x ^^^ ".cmi"
      end filenames
    in
    (* Convert filename to module name *)
    let module_names =
      List.map (fun basename -> String.capitalize_ascii (Filename.chop_suffix basename ".cmi"), path // basename) module_files
    in (module_names, Unix.gettimeofday ())
  ;;

  let read_memo = Memo.fast ~f:read';;

  let read ?(filter="") ~path () =
    let paths = path in
    List.fold_left begin fun acc path ->
      let force (_, ts) = try (Unix.stat path).Unix.st_mtime > ts with Unix.Unix_error _ -> false in
      let modules, _ = read_memo ~force (filter, path) in
      modules @ acc
    end [] paths
  ;;

  let get_descr ~project =
    let path = Project.get_load_path project in
    (*let path = project.Project.ocamllib :: path in*)
    let module_names = read ~path () in
    List.map begin fun (name, filename) ->
      let descr =
        let dirname = Filename.dirname filename in
        let in_source_path = project.Prj.in_source_path dirname <> None in
        if in_source_path then (sprintf "%s (project)" project.Prj.name)
        else if project.Prj.ocamllib = dirname then "StdLib"
        else begin
          match Miscellanea.filename_relative project.Prj.ocamllib dirname with
            | None -> filename
            | Some extra -> "+" ^ extra
        end
      in {mo_name=name; mo_filename=filename; mo_descr=descr};
    end module_names
  ;;
end


(** Read *)
module Signature = struct

  let kind_of_typekind = function
    | Type_variant _ -> Ptype_variant
    | Type_abstract -> Ptype_abstract
    | Type_record _ -> Ptype_record
    (* Since 4.02.0 - TODO ?? *)
    | Type_open -> Ptype

  let find_path modlid =
    Misc.find_in_path_uncap (Load_path.get_paths ()) (modlid^(".cmi"))

  let read_class_declaration ~filename ~parent_id ~id cd =
    let buf = Buffer.create 1024 in
    let formatter = Format.formatter_of_buffer buf in
    let print printtyp typ =
      Buffer.clear buf;
      printtyp typ;
      Format.pp_print_flush formatter ();
      (Buffer.contents buf)
    in
    (*match cd with {
      cty_params = cty_params;
      cty_type   = cty_type;
      cty_path   = cty_path; _
    } ->*)
      begin
        let [@warning "-4"] _ = "Disabel this pattern matching is fragile warning" in
        match Printtyp.tree_of_class_declaration id cd Types.Trec_first with
          | Osig_class (_(*vir_flag*), _(*name*), _(*params*), clt, _(*rs*)) ->
            let rec parse_class_type = function
              | Octy_signature (_(*self_ty*), csil) ->
                List.filter_map begin function
                  | Ocsg_method (name, priv, virt, ty) ->
                    Some {
                      sy_id           = parent_id @ [name];
                      sy_type         = sprintf "%s : %s" name (print (!Oprint.out_type formatter) ty);
                      sy_kind         =
                        if virt && priv then Pmethod_private_virtual
                        else if virt then Pmethod_virtual
                        else if priv then Pmethod_private
                        else Pmethod;
                      sy_filename     = filename;
                      sy_local        = false;
                    }
                  | _ -> None
                  (*| Ocsg_value (name, virt, priv, ty) -> None | Ocsg_constraint (t1, t2) -> None*)
                end csil
              | Octy_arrow (_, _, clt) -> parse_class_type clt
              | _ -> []
            in parse_class_type clt
          | _ -> []
      end
  ;;

  let rec read_module_type ~filename ~parent_longid = function
    | Mty_ident _(*path*) -> (* TODO: parse_module_type, Tmty_ident *)
      []
    | Mty_signature signature ->
      let modlid = match parent_longid with x :: _ -> x | _ -> "" in
      let symbols = read' (signature, filename, modlid) in
      let symbols = List.map (fun it -> {it with sy_id = parent_longid @ (List.tl it.sy_id)}) symbols in
      symbols
    | Mty_functor ( _, mc) -> read_module_type ~filename ~parent_longid mc
    (* Added in 4.02.0 *)
    | Mty_alias _  -> []

  and read' (sign, filename, modlid) =
    let buf = Buffer.create 1024 in
    let formatter = Format.formatter_of_buffer buf in
    let print kind id printtyp typ =
      Buffer.clear buf;
      printtyp typ;
      Format.pp_print_flush formatter ();
      let type_descr = Buffer.contents buf in
      Buffer.clear buf;
      Printtyp.ident formatter id;
      Format.pp_print_flush formatter ();
      let name = Buffer.contents buf in {
        sy_id       = [modlid; name];
        sy_kind     = kind;
        sy_type     = type_descr;
        sy_filename = filename;
        sy_local    = false;
      }
    in
    List.fold_left begin function acc -> function
      | Sig_value (id, value_description, _visibility) ->
        (print Pvalue id (Printtyp.value_description id formatter) value_description) :: acc;
      | Sig_type (id, type_declaration, _, _visibility) ->
        let acc =
          begin match type_declaration.type_manifest with
            | None ->
              let kind = kind_of_typekind type_declaration.type_kind in
              (print kind id (Printtyp.type_declaration id formatter) type_declaration) :: acc
            | Some te ->
              begin [@warning "-4"] match te.desc with
                | Tobject _ -> acc (* Niente definizione dei tipi oggetto *)
                | _ ->
                  let kind = kind_of_typekind type_declaration.type_kind in
                  (print kind id (Printtyp.type_declaration id formatter) type_declaration) :: acc
              end
          end
        in
        (* Costruttori di tipi varianti *)
        begin match type_declaration.type_kind with
          | Type_variant cc ->
            List.fold_left begin fun acc
                                     { cd_id = ident; cd_args = tel; cd_res = teo; _ } ->
              let n = Ident.name ident in
              let print_type_expr = print Pconstructor ident (Printtyp.type_expr formatter) in
              let is_gadt = match teo with Some _ -> true | None -> false in
              (* UGLY HACK *)
              let tel = match tel with Cstr_tuple ct -> ct | Cstr_record _ -> [] in
              let symbol =
                if List.length tel = 0 then
                  print Pconstructor ident ignore ()
                else begin
                  let items = List.map print_type_expr tel in
                  ListLabels.fold_left
                    ~f:begin fun acc it -> {
                      sy_id        = acc.sy_id;
                      sy_type      = acc.sy_type^" * "^it.sy_type;
                      sy_kind      = acc.sy_kind;
                      sy_filename  = filename;
                      sy_local     = false;
                    } end ~init:{
                      sy_id        = [modlid; n];
                      sy_type      = "";
                      sy_kind      = Pconstructor;
                      sy_filename  = filename;
                      sy_local     = false;
                    } items
                end
              in
              let d = replace_first ["^ \\* ", ""] symbol.sy_type in
              let is_bare = String.length d = 0 in
              let d = n ^ (if is_gadt then " : " else (if is_bare then "" else " of ")) ^ d in
              let d = match teo with Some te -> d ^ (if is_bare then "" else " -> ") ^ (print_type_expr te).sy_type | None -> d in
              {symbol with sy_type=d} :: acc
            end acc cc
          | _ -> acc
        end
      | Sig_typext (id, extension_constructor, _status, _visibility) ->
        (print Pexception id (Printtyp.extension_constructor id formatter)
          extension_constructor) :: acc;
      | Sig_module (id, _presence, module_declaration, _, _visibility) ->
        let module_item = print Pmodule id (Printtyp.ident formatter) id in
        let module_items = read_module_type
          ~filename  ~parent_longid:module_item.sy_id module_declaration.md_type
        in
        module_item :: module_items @ acc;
      | Sig_modtype (id, _, _visibility) ->
        (print Pmodtype id (Printtyp.ident formatter) id) :: acc;
      | Sig_class (id, class_declaration, _, _visibility) ->
        begin
          let class_item = print Pclass id (Printtyp.class_declaration id formatter) class_declaration in
          let class_items = read_class_declaration
            ~filename ~parent_id:class_item.sy_id ~id class_declaration in
          class_item :: class_items @ acc;
        end
      | Sig_class_type _(*(id, cltype_declaration, _, _visibility)*) -> acc
    end [] sign
  ;;

  let rec read'' (filename, modlid) =
    try
      let sign = Env.read_signature modlid filename in
      read' (sign, filename, modlid)
    with
      | (Persistent_env.Error e as exc) ->
        begin
          match e with
            | Persistent_env.Inconsistent_import _ ->
              Env.reset_cache();
              read'' ((*None, *)filename, modlid)
            | Persistent_env.Illegal_renaming _ -> raise Not_found
            | _ ->
              Printf.eprintf "File \"symbol.ml\": %s\n%s\n%!" (Printexc.to_string exc) (Printexc.get_backtrace());
              Persistent_env.report_error Format.err_formatter e;
              flush stderr;
              []
        end
      | Not_found | Sys_error _ -> []
      | Cmi_format.Error (Cmi_format.Not_an_interface msg) ->
        eprintf "Not_an_interface: %s\n" msg; []
      | Cmi_format.Error (Cmi_format.Wrong_version_interface (a, b)) ->
        eprintf "Wrong_version_interface: %s, %s\n" a b; []
      | Cmi_format.Error (Cmi_format.Corrupted_interface msg) ->
        eprintf "Corrupted_interface: %s\n" msg; []
  ;;

  (* filename is the .cmi file's full name *)
  let read ~cache ?filename ~modlid () =
    Mutex.lock cache.syt_critical;
    let result =
      try
        let filename = match filename with None -> find_path modlid | Some x -> x in
        let symbols = read'' (filename, modlid) in
        let ts = Unix.gettimeofday() in
        Hashtbl.replace cache.syt_ts filename ts;
        cache.syt_table <- List.filter (fun symbol ->
          (match symbol.sy_id with x :: _ -> x | _ -> assert false) <> modlid) cache.syt_table;
        cache.syt_table <- List.rev_append symbols cache.syt_table;
        symbols
      with
        | Not_found -> []
        | ex ->
          Printf.eprintf "File \"symbol.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
          raise ex
    in
    Mutex.unlock cache.syt_critical;
    result
  ;;
end


(** Cache *)
module Cache = struct

  let create_filename ~project =
    let cache_dir = Project.path_cache project in
    Miscellanea.mkdir_p cache_dir;
    cache_dir // "symbols";;

  let reset ~project =
    let name = (sprintf "Loading symbols (%s)..." project.Prj.name) in
    let finally () = GtkThread2.async Activity.remove name in
    ignore (Thread.create begin fun () ->
      GtkThread2.async (Activity.add Activity.Symbol) name;
      try
        let modules = Modules.get_descr ~project in
        List.iter begin fun md ->
          ignore (Signature.read
            ~cache:project.Prj.symbols
            ~filename:md.Modules.mo_filename
            ~modlid:md.Modules.mo_name ());
        end modules;
        finally()
      with ex -> begin
        Printf.eprintf "File \"symbol.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
        finally()
      end
    end ());;

  (* Update the part of the symbol cache relative to a specific module
     (check a single file.cmi). *)
  let update ~cache ~value_path () =
    match value_path with
      | modlid :: _ ->
        begin
          try
            begin
              let filename = try Signature.find_path modlid with Not_found -> raise Exit in
              try
                let last_modified = (Unix.stat filename).Unix.st_mtime in
                let last_read = try Hashtbl.find cache.syt_ts filename with Not_found -> 0.0 in
                let expired = last_read < last_modified in
                if expired then begin
                  ignore (Signature.read ~cache ~filename ~modlid ());
                  true
                end else false
              with Unix.Unix_error _ -> begin
                Hashtbl.remove cache.syt_ts filename;
                cache.syt_table <- List.filter (fun s -> s.sy_filename <> filename) cache.syt_table;
                true
              end
            end;
          with Exit -> begin
            let basename = (String.uncapitalize_ascii modlid) ^ ".cmi" in
            let remove = ref [] in
            Hashtbl.iter begin fun filename _ ->
              if Filename.basename filename = basename then (remove := filename :: !remove);
            end cache.syt_ts;
            List.iter (fun filename -> Hashtbl.remove cache.syt_ts filename) !remove;
            cache.syt_table <- List.filter (fun s -> Filename.basename s.sy_filename <> basename) cache.syt_table;
            true
          end
        end
      | _ -> false;;

  let save ~project =
    let filename = create_filename ~project in
    let chan = open_out_bin filename in
    let finally () = close_out chan in
    try
      let cache = Oe.Dump (Oe.magic, (project.Prj.symbols.Oe.syt_table, project.Prj.symbols.Oe.syt_ts)) in
      output_value chan cache;
      finally();
    with ex -> begin
      finally();
      raise ex
    end;;

  let load ~project =
    let filename = create_filename ~project in
    if Sys.file_exists filename then begin
      let chan = open_in_bin filename in
      let finally () = close_in chan in
      try
        let ((table, ts) : (symbol list * (string, float) Hashtbl.t)) = Oe.open_dump (input_value chan) in
        project.Prj.symbols.Oe.syt_table <- table;
        project.Prj.symbols.Oe.syt_ts <- ts;
        finally();
      with Bad_magic_number -> begin
        finally();
        reset ~project
      end | ex -> begin
        finally();
        raise ex
      end
    end else (reset ~project);;
end

(* find_parent *)
let find_parent (cache : symbol_cache) ?(update_cache=false) symbol =
  if update_cache then ignore (Cache.update ~cache ~value_path:symbol.sy_id ());
  let parent = get_parent_path symbol in
  let s_table = cache.syt_table in
  try Some (List.find (fun s -> s.sy_id = parent) s_table) with Not_found -> None;;

(* find_local_defs *)
let find_local_defs ~regexp ~(project : Prj.t) ~filename ~offset =
  let open Location in
  let open Lexing in
  let open Binannot in
  match Binannot_ident.find_local_definitions ~project ~filename () with
    | Some local_defs ->
      let defs =
        List.fold_left begin fun acc def ->
          if def.def_scope.loc_start.pos_cnum < offset && offset < def.def_scope.loc_end.pos_cnum && Str.string_match regexp def.def_name 0
          then def :: acc else acc
        end [] local_defs
      in
      let defs =
        List.fold_left begin fun acc def ->
          match List_opt.find (fun d -> d.def_name = def.def_name) acc with
            | Some d when d.def_scope.loc_start.pos_cnum > def.def_scope.loc_start.pos_cnum -> acc
            | Some d -> def :: (List.filter ((<>) d) acc)
            | None -> def :: acc
        end [] defs
      in
      let modlid = modname_of_path filename in
      List.map (fun def ->
        let typ =
          match Binannot_type.find_by_offset ~project ~filename ~offset:def.def_loc.loc_start.pos_cnum () with
            | Some typ -> typ.Binannot_type.ba_type
            | _ -> ""
        in {
          sy_id        = [modlid; def.def_name];
          sy_kind      = Pvalue;
          sy_type      = typ;
          sy_filename  = filename;
          sy_local     = true;
        }) defs
    | _ -> []
;;

(*
 * Cerca per corrispondenza esatta. In caso di modulo restituisce solo il risultato
 * relativo al modulo (senza i valori in esso contenuti).
 *)
let find_by_modulepath ?(kind=[]) (cache : symbol_cache) value_path =
  ignore (Cache.update ~cache ~value_path ());
  let s_table = cache.syt_table in
  let exclude_kind = [Pmethod; Pmethod_private; Pmethod_virtual; Pmethod_private_virtual] in
  try
    let symbol =
      List.find begin fun symbol ->
        (kind = [] || List.mem symbol.sy_kind kind) && not (List.mem symbol.sy_kind exclude_kind) &&
        (List_opt.for_all2 (=) value_path symbol.sy_id)
      end s_table
    in
    Some symbol
  with Not_found -> None
;;

(* filter_methods *)
let filter_methods (cache : symbol_cache) class_path =
  ignore (Cache.update ~cache ~value_path:class_path ());
  let stable = cache.syt_table in
  List.filter begin fun symbol ->
    symbol.sy_kind = Pmethod && begin
      let matches = ref true in
      ignore (List_opt.for_all2 (fun a b -> matches := !matches && a = b; true) class_path symbol.sy_id);
      !matches;
    end
  end stable
;;

(* filter_by_name *)
let filter_by_name
    ?(use_longidents=true)
    ?include_locals
    ?(include_methods=true)
    ?include_modules
    ~regexp
    symbols =
  let result =
    match use_longidents with
      | true ->
        (*let matches s = List.exists (fun part -> Str.string_match regexp part 0) in*)
        List.filter begin fun symbol ->
          (*(include_methods || not (is_method symbol)) && matches symbol.sy_id*)
          (include_methods || not (is_method symbol)) && (Str.string_match regexp (concat_value_path symbol) 0)
        end symbols
      | false ->
        List.filter begin fun symbol ->
          (include_methods || not (is_method symbol)) && Str.string_match regexp (get_name symbol) 0
        end symbols
  in
  let result =
    match include_modules with
      | Some path ->
        let modules = Modules.read ~path () in
        let modules = List.filter_map begin fun (m, filename) ->
          if Str.string_match regexp m 0
          then Some {sy_id=[m]; sy_kind=Pmodule; sy_type=m; sy_filename=filename; sy_local=false}
          else None;
        end modules in
        modules @ result
      | _ -> result
  in
  let result =
    match include_locals with
      | Some (project, filename, offset) ->
        let locals = find_local_defs ~regexp ~project ~filename ~offset in
        locals @ result
      | _ -> result
  in
  result
;;

(* filter_by_type *)
let filter_by_type ~regexp symbols =
  List.filter begin fun symbol ->
      let start = try String.index symbol.sy_type ':' with Not_found -> 0 in
      try Str.search_forward regexp symbol.sy_type start |> ignore; true with Not_found -> false
    end symbols

(*
 * Trova tutti i simboli che iniziano con value_path; nel caso di un modulo
 * restituisce anche i valori in esso contenuti.
 *)
let filter_by_modulepath ?(update_cache=true) (cache : symbol_cache) value_path =
  if update_cache then (ignore (Cache.update ~cache ~value_path ()));
  let s_table = cache.syt_table in
  let exclude_kind = [Pmethod; Pmethod_private; Pmethod_virtual; Pmethod_private_virtual] in
  List.filter begin fun symbol ->
    not (List.mem symbol.sy_kind exclude_kind) &&
    let matches = ref true in
    ignore (List_opt.for_all2 (fun a b -> matches := !matches && a = b; true) value_path symbol.sy_id);
    !matches;
  end s_table
;;




