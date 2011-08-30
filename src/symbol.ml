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


open Types
open Outcometree
open Oe
open Miscellanea
open Printf

type module_descr = {
  mo_name     : string;
  mo_filename : string;
  mo_descr    : string;
}

exception Symbol_cache_busy

(** get_module_path *)
let get_module_path symbol =
  try List.rev (List.tl (List.rev symbol.sy_id))
  with Failure "tl" -> []

(** get_name *)
let get_name symbol =
  try List.hd (List.rev symbol.sy_id) with Failure "hd" -> assert false

(** get_module_name *)
let get_module_name symbol =
  try List.hd symbol.sy_id with Failure "hd" -> assert false

(** is_method *)
let is_method symbol =
  match symbol.sy_kind with
    | Pmethod | Pmethod_private | Pmethod_virtual | Pmethod_private_virtual -> true
    | _ -> false

(** concat_value_path *)
let concat_value_path symbol =
  if is_method symbol then begin
    let rec f acc = function
      | [] -> acc
      | a :: b :: [] -> acc ^ "." ^ a ^ "#" ^ b
      | a :: tl -> f (acc ^ a) tl
    in f "" symbol.sy_id
  end else (String.concat "." symbol.sy_id)

(** string_of_id *)
let string_of_id = String.concat "."

(** find_path *)
let find_path modlid = Misc.find_in_path_uncap !Config.load_path (modlid^(".cmi"))

(** read_modules_name *)
let read_modules_name' (filter, sort, path) =
  let len = String.length filter in
  let filter = String.lowercase filter in
  (* Read .cmi files from path *)
  let filenames = Array.to_list (try Sys.readdir path with (Sys_error _) -> [||]) in
  let module_files =
    List.filter begin fun x ->
      (len = 0 || String.length x >= len) &&
      (String.lowercase (Str.first_chars x len)) = filter (* TODO: improve *)
      && Filename.check_suffix x ".cmi"
    end filenames
  in
  (* Convert filename to module name *)
  let module_names =
    List.map (fun basename -> String.capitalize (Filename.chop_suffix basename ".cmi"), path // basename) module_files
  in (module_names, Unix.gettimeofday ())
;;

let read_modules_name_memo = Memo.fast ~f:read_modules_name';;

let read_modules_name ?(filter="") ?(sort=false) ~path () =
  let paths = path in
  List.fold_left begin fun acc path ->
    let force (_, ts) = try (Unix.stat path).Unix.st_mtime > ts with Unix.Unix_error _ -> false in
    let modules, _ = read_modules_name_memo ~force (filter, sort, path) in
    modules @ acc
  end [] paths
;;

(** get_modules_descr *)
let get_modules_descr ~project =
  let path = Project.get_load_path project in
  let ocamllib = project.Project.ocamllib in
  let ocamllib = if ocamllib = "" then Ocaml_config.ocamllib () else ocamllib in
  let path = ocamllib :: path in
  let module_names = read_modules_name ~path () in
  List.map begin fun (name, filename) ->
    let descr =
      let dirname = Filename.dirname filename in
      let in_source_path = project.Project.in_source_path dirname <> None in
      if in_source_path then (sprintf "%s (project)" project.Project.name)
      else if project.Project.ocamllib = dirname then "StdLib"
      else begin
        match Miscellanea.filename_relative project.Project.ocamllib dirname with
          | None -> filename
          | Some extra -> "+" ^ extra
      end
    in {mo_name=name; mo_filename=filename; mo_descr=descr};
  end module_names
;;

(** read_class_declaration *)
let read_class_declaration ~filename ~parent_id ~id cd =
  let buf = Buffer.create 1024 in
  let formatter = Format.formatter_of_buffer buf in
  let print id printtyp typ =
    Buffer.clear buf;
    printtyp typ;
    Format.pp_print_flush formatter ();
    (Buffer.contents buf)
  in
  let dummy_id = Ident.create "" in
  match cd with {
    cty_params = cty_params;
    cty_type = cty_type;
    cty_path = cty_path
  } ->
    begin
      match Printtyp.tree_of_class_declaration id cd Types.Trec_first with
        | Osig_class (vir_flag, name, params, clt, rs) ->
          let rec parse_class_type = function
            | Octy_signature (self_ty, csil) ->
              Miscellanea.Xlist.filter_map begin function
                | Ocsg_method (name, priv, virt, ty) ->
                  Some {
                    sy_id           = parent_id @ [name];
                    sy_type         = sprintf "%s : %s" name (print dummy_id (!Oprint.out_type formatter) ty);
                    sy_kind         =
                      if virt && priv then Pmethod_private_virtual
                      else if virt then Pmethod_virtual
                      else if priv then Pmethod_private
                      else Pmethod;
                    sy_filename     = filename;
                  }
                | _ -> None
                (*| Ocsg_value (name, virt, priv, ty) -> None | Ocsg_constraint (t1, t2) -> None*)
              end csil
            | Octy_fun (_, _, clt) -> parse_class_type clt
            | _ -> []
          in parse_class_type clt
        | _ -> []
    end
;;

(** read_module_type *)
let rec read_module_type ~filename ~parent_longid = function
  | Tmty_ident path -> (* TODO: parse_module_type, Tmty_ident *)
    []
  | Tmty_signature signature ->
    let modlid = try List.hd parent_longid with Failure "hd" -> "" in
    let symbols = read_signature' (signature, filename, modlid) in
    let symbols = List.map (fun it -> {it with sy_id = parent_longid @ (List.tl it.sy_id)}) symbols in
    symbols
  | Tmty_functor (ident, md, mc) -> read_module_type filename parent_longid mc

(** read_signature' *)
and read_signature' (sign, filename, modlid) =
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
    }
  in
  let result =
    List.fold_left begin function acc -> function
      | Tsig_value (id, value_description) ->
        (print Pvalue id (Printtyp.value_description id formatter) value_description) :: acc;
      | Tsig_type (id, type_declaration, _) ->
        let acc =
          begin match type_declaration.type_manifest with
            | None ->
              (print Ptype id (Printtyp.type_declaration id formatter) type_declaration) :: acc
            | Some te ->
              begin match te.desc with
                | Tobject (te, me) -> acc (* Niente definizione dei tipi oggetto *)
                | _ -> (print Ptype id (Printtyp.type_declaration id formatter) type_declaration) :: acc
              end
          end
        in
        (* Costruttori di tipi varianti *)
        begin match type_declaration.type_kind with
          | Type_variant cc ->
            List.fold_left begin fun acc (n, tel) ->
              let symbol =
                if List.length tel = 0 then
                  print Pconstructor (Ident.create n) ignore ()
                else begin
                  let items = List.map (print Pconstructor (Ident.create n) (Printtyp.type_expr formatter)) tel in
                  ListLabels.fold_left
                    ~f:begin fun acc it -> {
                      sy_id        = acc.sy_id;
                      sy_type      = acc.sy_type^" * "^it.sy_type;
                      sy_kind      = acc.sy_kind;
                      sy_filename  = filename;
                    } end ~init:{
                      sy_id        = [modlid; n];
                      sy_type      = "";
                      sy_kind      = Pconstructor;
                      sy_filename  = filename;
                    } items
                end
              in
              let d = replace_first ["^ \\* ", ""] symbol.sy_type in
              let d = n^(if String.length d > 0 then " of "^d else "") in
              let d = d ^ " : " ^ (Ident.name id) in
              {symbol with sy_type=d} :: acc
            end acc cc
          | _ -> acc
        end
      | Tsig_exception (id, exception_declaration) ->
        (print Pexception id (Printtyp.exception_declaration id formatter)
          exception_declaration) :: acc;
      | Tsig_module (id, module_type, _) ->
        let module_item = print Pmodule id (Printtyp.ident formatter) id in
        let module_items = read_module_type
          ~filename  ~parent_longid:module_item.sy_id module_type
        in
        module_item :: module_items @ acc;
      | Tsig_modtype (id, _) ->
        (print Pmodtype id (Printtyp.ident formatter) id) :: acc;
      | Tsig_class (id, class_declaration, _) ->
        begin
          let class_item = print Pclass id (Printtyp.class_declaration id formatter) class_declaration in
          let class_items = read_class_declaration
            ~filename ~parent_id:class_item.sy_id ~id class_declaration in
          class_item :: class_items @ acc;
        end
      | Tsig_cltype (id, cltype_declaration, _) -> acc
    end [] sign
  in result
;;

(** read_signature'' *)
let rec read_signature'' (filename, modlid) =
  try
    let sign = Env.read_signature modlid filename in
    read_signature' (sign, filename, modlid)
  with
    | (Env.Error e as exc) ->
      begin
        match e with
          | Env.Inconsistent_import _ ->
            Env.reset_cache();
            read_signature'' ((*None, *)filename, modlid)
          | Env.Not_an_interface _ -> raise Not_found
          | _ ->
            Printf.eprintf "File \"symbol.ml\": %s\n%s\n%!" (Printexc.to_string exc) (Printexc.get_backtrace());
            Env.report_error Format.err_formatter e;
            flush stderr;
            []
      end
    | Not_found | Sys_error _ -> []
;;

(** read_signature
    filename is the .cmi file's full name.
  *)
let read_signature ~cache ?filename ~modlid () =
  Mutex.lock cache.syt_critical;
  let result =
    try
      let filename = match filename with None -> find_path modlid | Some x -> x in
      let symbols = read_signature'' (filename, modlid) in
      let ts = Unix.gettimeofday() in
      Hashtbl.replace cache.syt_ts filename ts;
      cache.syt_table <- List.filter (fun symbol -> List.hd symbol.sy_id <> modlid) cache.syt_table;
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

(** reset_cache *)
let reset_cache ~project =
  let name = (sprintf "Loading symbols (%s)..." project.Project.name) in
  let finally () = GtkThread2.async Activity.remove name in
  ignore (Thread.create begin fun () ->
    GtkThread2.async (Activity.add Activity.Symbol) name;
    try
      let modules = get_modules_descr ~project in
      List.iter begin fun md ->
        (*Printf.printf "md.mo_filename = %s\n%!" md.mo_filename;*)
        ignore (read_signature
          ~cache:project.Project.symbols
          ~filename:md.mo_filename
          ~modlid:md.mo_name ());
      end modules;
      finally()
    with ex -> begin
      Printf.eprintf "File \"symbol.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
      finally()
    end
  end ())
;;

(** update_cache
  * Update the part of the symbol cache relative to a specific module
  * (check a single file.cmi).
  *)
let update_cache ~cache ~value_path () =
  try
    begin
      let modlid = List.hd value_path in
      try
        begin
          let filename = try find_path modlid with Not_found -> raise Exit in
          try
            let last_modified = (Unix.stat filename).Unix.st_mtime in
            let last_read = try Hashtbl.find cache.syt_ts filename with Not_found -> 0.0 in
            let expired = last_read < last_modified in
            if expired then begin
              ignore (read_signature ~cache ~filename ~modlid ())
            end
          with Unix.Unix_error _ -> begin
            Hashtbl.remove cache.syt_ts filename;
            cache.syt_table <- List.filter (fun s -> s.sy_filename <> filename) cache.syt_table;
          end
        end;
      with Exit -> begin
        let basename = (String.uncapitalize modlid) ^ ".cmi" in
        let remove = ref [] in
        Hashtbl.iter begin fun filename _ ->
          if Filename.basename filename = basename then (remove := filename :: !remove);
        end cache.syt_ts;
        List.iter (fun filename -> Hashtbl.remove cache.syt_ts filename) !remove;
        cache.syt_table <- List.filter (fun s -> Filename.basename s.sy_filename <> basename) cache.syt_table;
      end
    end;
  with Failure "hd" -> ()
;;

(** find_by_modulepath
  * Cerca per corrispondenza esatta. Nel caso di un modulo restituisce solo l'entry
  * relativa al modulo (senza i valori in esso contenuti).
  *)
let rec find_by_modulepath ?(kind=[]) (cache : symbol_cache) value_path =
  update_cache ~cache ~value_path ();
  let stable = cache.syt_table in
  let exclude_kind = [Pmethod; Pmethod_private; Pmethod_virtual; Pmethod_private_virtual] in
  try
    let symbol =
      List.find begin fun symbol ->
        (List.mem symbol.sy_kind kind) && not (List.mem symbol.sy_kind exclude_kind) &&
        (try List.for_all2 (=) value_path symbol.sy_id with Invalid_argument _ -> false)
      end stable
    in
    Some symbol
  with Not_found -> None
;;

(** filter_methods *)
let rec filter_methods (cache : symbol_cache) class_path =
  update_cache ~cache ~value_path:class_path ();
  let stable = cache.syt_table in
  List.filter begin fun symbol ->
    symbol.sy_kind = Pmethod && begin
      let matches = ref true in
        try
          ignore (List.for_all2 (fun a b -> matches := !matches && a = b; true) class_path symbol.sy_id);
          !matches;
        with Invalid_argument _ -> !matches
    end
  end stable
;;

(** filter_by_name *)
let rec filter_by_name (cache : symbol_cache) (*?(limit=500)*) ?(case_sensitive=false) name =
  let stable = cache.syt_table in
  let re = (if case_sensitive then Str.regexp else Str.regexp_case_fold) (sprintf ".*%s.*" name) in
  (*let count = ref 0 in*)
  let matches path =
    (*!count < limit && *)List.exists (fun part -> Str.string_match re part 0) path
  in
  List.filter begin fun symbol -> matches symbol.sy_id
   (* if matches symbol.sy_id then begin
      incr count;
      true
    end else false*)
  end stable
;;

(** filter_by_modulepath
  * Trova tutti i simboli che iniziano con value_path; nel caso di un modulo
  * restituisce anche i valori in esso contenuti.
  *)
let rec filter_by_modulepath (cache : symbol_cache) value_path =
  update_cache ~cache ~value_path ();
  let stable = cache.syt_table in
  let exclude_kind = [Pmethod; Pmethod_private; Pmethod_virtual; Pmethod_private_virtual] in
  List.filter begin fun symbol ->
    not (List.mem symbol.sy_kind exclude_kind) &&
    let matches = ref true in
      try
        ignore (List.for_all2 (fun a b -> matches := !matches && a = b; true) value_path symbol.sy_id);
        !matches;
      with Invalid_argument _ -> !matches
  end stable
;;





