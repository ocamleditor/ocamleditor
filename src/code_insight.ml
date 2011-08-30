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
open Longident
open Miscellanea
open Printf
open Outcometree

type t = {
  ci_mode   : mode;
  ci_text   : string;
  ci_length : int;
  mutable ci_title  : string;
  mutable ci_result : item list;
}

and item = {
  ci_modlid : string; (* module-name, e.g. C in A.B.C.x *)
  ci_name   : string; (* value-name *)
  ci_descr  : string;
  ci_kind   : pkind;
  ci_local  : bool;
  mutable ci_cdecl  : (string * Ident.t * Types.class_declaration) option; (* modlid * classname * type *)
  mutable ci_mtype : (string * Types.module_type) option; (* modlid * module_type *)
  mutable ci_longid : string list; (* value-path *)
}

and mode = Ci_symbol | Ci_module of string | Ci_class of string option

and pkind =
    Pvalue
  | Pfunc
  | Pmethod | Pmethod_private | Pmethod_virtual | Pmethod_private_virtual
  | Ptype
  | Plabel
  | Pconstructor
  | Pexception
  | Pmodule
  | Pmodtype
  | Pclass
  | Pcltype

(** string_of_kind *)
let string_of_kind = function
    Pvalue -> "val"
  | Pfunc -> "func"
  | Pmethod -> "method"
  | Pmethod_virtual -> "method virtual"
  | Pmethod_private -> "method private"
  | Pmethod_private_virtual -> "method private virtual"
  | Ptype -> "type"
  | Plabel -> "label"
  | Pconstructor -> "constructor"
  | Pexception -> "exception"
  | Pmodule -> "module"
  | Pmodtype -> "module type"
  | Pclass -> "class"
  | Pcltype -> "class type"

(** find_path *)
let find_path modlid = Misc.find_in_path_uncap !Config.load_path (modlid^(".cmi"))

(** read_module_names *)
let read_module_names' (filter, sort, path) =
  let len = String.length filter in
  let filter = String.lowercase filter in
  (* Read .cmi files in path *)
  let module_names =
    List.filter begin fun x ->
      (len = 0 || String.length x >= len) &&
      (String.lowercase (Str.first_chars x len)) = filter (* TODO: improve *)
      && Filename.check_suffix x ".cmi"
    end (Array.to_list (try Sys.readdir path with (Sys_error _) -> [||]))
  in
  (* Convert filename to module name *)
  let module_names =
    List.map (fun x -> String.capitalize (Filename.chop_suffix x ".cmi")) module_names
  in (module_names, Unix.gettimeofday ())
;;

let read_module_names_memo = Memo.fast ~f:read_module_names';;

let read_module_names ?(filter="") ?(sort=false) ~path () =
  let paths = path in
  List.fold_left begin fun acc path ->
    let force (_, ts) = try (Unix.stat path).Unix.st_mtime > ts with Unix.Unix_error _ -> false in
    let module_names, _ = read_module_names_memo ~force (filter, sort, path) in
    (List.map (fun m -> m, path) module_names) @ acc
  end [] paths
;;

(** parse_filter *)
let parse_filter ~project ~filename ~(iter : GText.iter) ~text =
  if Str.string_match (Miscellanea.regexp "\\([A-Z][A-Za-z_0-9']*\\)[.]\\(.*\\)") text 0 then begin
    let module_name = Str.matched_group 1 text in
    let search = String.lowercase (Str.matched_group 2 text) in
    let len = String.length search in
    {ci_mode=Ci_module module_name; ci_text=search; ci_length=len; ci_title=module_name; ci_result=[]}
  end else if Str.string_match (Miscellanea.regexp "\\(.*\\)\\([a-z_0-9']+\\)#\\([a-z_0-9']*\\)") text 0 then begin
    let obj_name = Str.matched_group 2 text in
    let offset = (iter#backward_find_char (fun x -> Glib.Utf8.from_unichar x = "#"))#backward_char#offset in
    let annot = Annotation.find_block_at_offset ~project ~filename ~offset in
    let class_type =
      match annot with
        | None -> (* try to lookup obj_name in the ident. table *)
          let obj_types =
            Miscellanea.Xlist.filter_map begin fun ((f, n), (dstop, dtype)) ->
              if f = filename && n = obj_name then begin
                match dstop with
                  | None -> Some dtype
                  | Some stop when offset <= stop.Oe.annot_cnum ->
                    Some dtype
                  | _ -> None
              end else None
            end !Annotation.itable
          in
          (* TODO: exculde non-class types from the list *)
          (* The head of the list is the last defined name *)
          (match obj_types with [] -> None | x -> List.hd x)
        | Some block -> Annotation.get_type block.Oe.annot_annotations
    in
    let search = String.lowercase (Str.matched_group 3 text) in
    let len = String.length search in
    {ci_mode=Ci_class class_type; ci_text=search; ci_length=len; ci_title=""; ci_result=[]}
  end else {ci_mode=Ci_symbol; ci_text=(String.lowercase text); ci_length=String.length text; ci_title=""; ci_result=[]}
;;

(** parse_class_declaration *)
let parse_class_declaration modlid id cd =
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
                    ci_modlid    = modlid;
                    ci_name      = name;
                    ci_longid    = [];
                    ci_descr     = sprintf "%s : %s" name (print dummy_id (!Oprint.out_type formatter) ty);
                    ci_kind      =
                      if virt && priv then Pmethod_private_virtual
                      else if virt then Pmethod_virtual
                      else if priv then Pmethod_private
                      else Pmethod;
                    ci_local     = false;
                    ci_cdecl     = None;
                    ci_mtype = None;
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

(** parse_module_type *)
let rec parse_module_type parent_longid = function
  | Tmty_ident path -> (* TODO: parse_module_type, Tmty_ident *)
    Printf.eprintf "Assertion failed: parse_module_type, Tmty_ident\n%!";
    assert false;
  | Tmty_signature signature ->
    let modlid = try List.hd parent_longid with Failure "hd" -> "" in
    let items, ts = signature' (signature, modlid) in
    List.iter (fun it -> it.ci_longid <- it.ci_name :: parent_longid) items;
    items, ts
  | Tmty_functor (ident, md, mc) -> parse_module_type parent_longid mc

(** signature' *)
and signature' (sign, modlid) =
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
    let name = Buffer.contents buf in
    { ci_modlid = modlid;
      ci_name   = name;
      ci_longid = [name; modlid];
      ci_descr  = type_descr;
      ci_kind   = kind;
      ci_cdecl  = None;
      ci_mtype  = None;
      ci_local  = false;
    }
  in
  let result =
    List.fold_left begin function acc -> function
      | Tsig_value (id, value_description) ->
        (print Pvalue id (Printtyp.value_description id formatter) value_description) :: acc;
      | Tsig_type (id, type_declaration, _) ->
        let acc = begin match type_declaration.type_manifest with
          | None ->
            (print Ptype id (Printtyp.type_declaration id formatter)
              type_declaration) :: acc
          | Some te -> begin match te.desc with
            | Tobject (te, me) -> acc (* Niente definizione dei tipi oggetto *)
            | _ -> (print Ptype id (Printtyp.type_declaration id formatter)
              type_declaration) :: acc
          end
        end in (* Costruttori dei tipi varianti *)
        begin match type_declaration.type_kind with
          | Type_variant cc ->
            List.fold_left begin fun acc (n, tel) ->
              let sign =
                if List.length tel = 0 then
                  print Pconstructor (Ident.create n) ignore ()
                else begin
                  let items = List.map (print Pconstructor (Ident.create n) (Printtyp.type_expr formatter)) tel in
                  ListLabels.fold_left
                    ~f:begin fun acc it -> {
                      ci_modlid = acc.ci_modlid;
                      ci_name   = acc.ci_name;
                      ci_longid = acc.ci_longid;
                      ci_descr  = acc.ci_descr^" * "^it.ci_descr;
                      ci_kind   = acc.ci_kind;
                      ci_cdecl  = acc.ci_cdecl;
                      ci_mtype  = None;
                      ci_local  = false
                    } end ~init:{
                      ci_modlid = modlid;
                      ci_name   = n;
                      ci_longid = [];
                      ci_descr  = "";
                      ci_kind   = Pconstructor;
                      ci_cdecl  = None;
                      ci_mtype  = None;
                      ci_local  = false
                    } items
                end
              in
              let d = replace_first ["^ \\* ", ""] sign.ci_descr in
              let d = n^(if String.length d > 0 then " of "^d else "") in
              let d = d ^ " : " ^ (Ident.name id) in
              {sign with ci_name=n; ci_descr=d} :: acc
            end acc cc
          | _ -> acc
        end
      | Tsig_exception (id, exception_declaration) ->
        (print Pexception id (Printtyp.exception_declaration id formatter)
          exception_declaration) :: acc;
      | Tsig_module (id, module_type, _) ->
        let module_item = print Pmodule id (Printtyp.ident formatter) (Ident.create "<module>") in
        module_item.ci_mtype <- Some (Ident.name id, module_type);
        module_item :: acc;
      | Tsig_modtype (id, _) ->
        (print Pmodtype id (Printtyp.ident formatter) (Ident.create "<module type>")) :: acc;
      | Tsig_class (id, class_declaration, _) ->
        begin
          let class_item = print Pclass id (Printtyp.class_declaration id formatter) class_declaration in
          class_item.ci_cdecl <- Some (modlid, id, class_declaration);
          class_item :: acc;
        end
      | Tsig_cltype (id, cltype_declaration, _) -> acc
  end [] sign in
  result, Unix.gettimeofday()

(** signature'' *)
let rec signature'' ((sign : Types.signature option), filename, modlid) =
  match sign with
    | None ->
      begin
        try
          let sign = Env.read_signature modlid filename in
          signature' (sign, modlid)
        with (Env.Error e as exc) ->
          begin
            Env.report_error Format.std_formatter e;
            print_newline();
            match e with
              | Env.Inconsistent_import _ ->
                Env.reset_cache();
                signature'' (None, filename, modlid);
              | Env.Not_an_interface _ -> raise Not_found
              | _ -> raise exc
          end
        | Not_found -> [], Unix.gettimeofday()
      end
    | Some sign -> signature' (sign, modlid)


(** signature_memo *)
let signature_memo = (Memo.fast ~f:signature'')

(** signature *)
let signature ?path ?sign modlid =
  try
    let path = match path with None -> find_path modlid | Some p -> p in
    let force = fun (_, ts) -> (Unix.stat path).Unix.st_mtime > ts in
    let items, _ = signature_memo ~force (sign, path, modlid) in
    items
  with Not_found -> []

(** find_local_defs *)
let find_local_defs ~(project : Project.t) ~filename ~offset =
  match Annotation.find ~project ~filename () with
    | None -> []
    | Some an ->
      let blocks = an.Oe.annot_blocks in
      let defs =
        (* start non comprende gli spazi bianchi e commenti *)
        List.map begin fun block ->
          match Annotation.get_def block.Oe.annot_annotations with
            | None -> None
            | Some (name, start, stop) ->
              let typ =
                match Annotation.get_type block.Oe.annot_annotations with
                  | None -> "???"
                  | Some typ -> typ
              in
              let stop = match stop with Some stop -> stop.Oe.annot_cnum | _ -> max_int in
              if start.Oe.annot_cnum <= offset && offset < stop
              then (Some (name, typ))
              else None
        end blocks
      in
      let modlid = modname_of_path filename in
      List.map (fun (name, typ) -> {
        ci_modlid = modlid;
        ci_name   = name;
        ci_longid = [name; modlid];
        ci_descr  = typ;
        ci_kind   = Pvalue;
        ci_cdecl  = None;
        ci_mtype  = None;
        ci_local  = true
      }) (Miscellanea.Xlist.filter_map (fun x -> x) defs)
;;

(** find_method_completion *)
let find_method_completion ~insight ~class_type =
  let lident = class_type in
  let lident = Longident.flatten (Longident.parse lident) in
  begin
    match lident with
      | module_name :: rest when rest <> [] ->
        (* Strip leading type parameters from class type *)
        let module_name =
          try Str.string_after module_name (String.rindex module_name ' ' + 1)
          with Not_found -> module_name
        in
        begin
          try
            let class_name = List.hd (List.rev rest) in
            let sign = signature module_name in
            let cdecl = List.find (fun entry -> entry.ci_kind = Pclass && entry.ci_name = class_name) sign in
            insight.ci_title <- sprintf "%s.%s" module_name class_name;
            begin
              match cdecl.ci_cdecl with
                | Some (modlid, class_name, class_decl) ->
                  parse_class_declaration modlid class_name class_decl
                | _ -> []
            end;
          with Failure "hd" | Not_found -> []
        end;
      | local_class_name :: [] -> []
      | _ -> []
  end
;;

(** find_modules *)
let find_modules ~project ~mode =
  let modules =
    match mode with Ci_symbol -> (read_module_names ~path:!Config.load_path ()) | _ -> []
  in
  List.map begin fun (m, path) ->
    let descr =
      if project.Project.in_source_path path <> None then (sprintf "%s (project)" project.Project.name)
      else if project.Project.ocamllib = path then "StdLib"
      else begin
        match Miscellanea.filename_relative project.Project.ocamllib path with
          | None -> path
          | Some extra -> "+" ^ extra
      end
    in {
      ci_modlid = "";
      ci_name   = m;
      ci_longid = [m];
      ci_descr  = descr;
      ci_kind   = Pmodule;
      ci_cdecl  = None;
      ci_mtype  = None;
      ci_local  = false
    }
  end modules
;;

(** find_symbol *)
let rec find_symbol' longid = function
  | [] -> None
  | ({ci_modlid=mi; ci_name=id; ci_mtype=mtype; ci_kind=kind} as item) :: rest ->
    begin
      (*Printf.printf "*** %s.%s\n%!" mi id;*)
      match longid with
        | a :: b :: [] when (List.mem kind [Ptype; Pclass; Pconstructor]) && a = mi && b = id -> Some item
        | a :: b :: _ when (List.mem kind [Pmodule]) && a = mi && b = id ->
          let submodule_items, _ =
            match mtype with
              | None -> assert false
              | Some (m, ty) -> parse_module_type item.ci_longid ty
          in
          (*Printf.printf "submodule_items --------------> %s.%s -- %s -- %s\n%!" mi id (String.concat "." longid) (String.concat "." item.ci_longid);*)
          find_symbol' (List.tl longid) submodule_items;
        | _ -> find_symbol' longid rest
    end
;;

let find_symbol name paths =
  let modules = read_module_names ~path:paths () in
  let rec find name paths =
    let longid = Longident.flatten (Longident.parse name) in
    let module_name = try List.hd longid with Failure "hd" -> assert false in
    try
      let module_name, module_path = List.find (fun (x, _) -> x = module_name) modules in
      let filename = module_path // ((String.uncapitalize module_name) ^ ".cmi") in
      (*Printf.printf "name = %S; module_name = %s; filename = %s\n%!" name module_name filename;*)
      let items = signature ~path:filename module_name in
      let item = find_symbol' longid items in
      item
    with Not_found -> None
  in
  match find name paths with
    | None ->
      begin
        try
          let longid = Longident.flatten (Longident.parse name) in
          let name = List.hd (List.rev longid) in
          let name = "Pervasives." ^ name in
          find name paths
        with Failure "hd" -> None
      end
    | x -> x
;;

(** preload_signatures *)
let preload_signatures ~project = ()
(*  let modules = find_modules ~project ~mode:Ci_symbol in
  let modules = ref modules in
  let name = "Loading signatures..." in
  Gtk_util.background_process_add name;
  ignore (GMain.Timeout.add ~ms:70 ~callback:begin fun () ->
    match !modules with
      | [] -> Gtk_util.background_process_remove name; false
      | {ci_name=m} :: tl ->
        modules := tl;
        ignore (signature m);
        true
  end)*)
;;

(** create *)
let create ~text (page : Editor_page.page)  =
  if text = "" then None else begin
    let project = page#project in
    let iter = page#buffer#get_iter `INSERT in
    let offset = iter#offset in
    let filename = page#get_filename in
    let insight = parse_filter ~project ~filename ~iter ~text in
    (** Find modules *)
    let modules = find_modules ~project ~mode:insight.ci_mode in
    (** Parse signatures *)
    let possibilities =
      match insight.ci_mode with
        | Ci_module module_name -> signature module_name
        | Ci_class (Some class_type) -> find_method_completion ~insight ~class_type
        | _ ->
          List.fold_left
            (fun sign modlid -> (signature modlid) @ sign) (signature "Pervasives")
            (List.map (fun {ci_name=m} -> m) modules)
    in
    (** Find local definitions *)
    let locals =
      match insight.ci_mode with Ci_symbol _ -> find_local_defs ~project ~filename ~offset | _ -> []
    in
    (** Concat *)
    let possibilities = possibilities @ modules @ locals in
    (** Filter possibilities *)
    let possibilities =
      match insight.ci_mode with
        | Ci_symbol _ ->
          List.filter begin fun x ->
            String.length x.ci_name >= insight.ci_length &&
            Str.first_chars (String.lowercase x.ci_name) insight.ci_length = insight.ci_text
          end possibilities
        | _ -> possibilities
    in
    (** Sort *)
    let possibilities =
      List.sort begin fun x1 x2 ->
        let comp = Pervasives.compare (String.lowercase x1.ci_name) (String.lowercase x2.ci_name) in
        if comp = 0 then Pervasives.compare x1.ci_modlid x2.ci_modlid else comp
      end possibilities
    in
    insight.ci_result <- possibilities;
    Some insight
  end





