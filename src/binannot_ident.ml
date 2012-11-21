(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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
open Cmt_format
open Typedtree
open Location
open Lexing
open Binannot

module Log = Common.Log.Make(struct let prefix = "Binannot_ident" end)
let _ = Log.set_verbosity `DEBUG

exception Found of ident

type ext_def = Project_def of ident | Library_def

type search_result = {
  bai_def  : ident option;
  bai_refs : ident list;
}

(** iter_pattern *)
let rec iter_pattern f {pat_desc; pat_loc; pat_type; pat_extra; _} =
  let fp = iter_pattern f in
  match pat_desc with
    | Tpat_tuple pl ->
      List.flatten (List.fold_left (fun acc pat -> (fp pat) :: acc) [] pl)
    | Tpat_alias (pat, _, _) ->
      fp pat
    | Tpat_construct (_, loc, _, pl, _) ->
      List.flatten (List.fold_left (fun acc pat -> (fp pat) :: acc) [] pl)
    | Tpat_variant (lab, pat, _) ->
      Opt.map_default pat [] (fun pat -> fp pat)
    | Tpat_record (ll, _) ->
      List.flatten (List.fold_left (fun acc (_, _, _, pat) -> (fp pat) :: acc) [] ll)
    | Tpat_array pl ->
      List.flatten (List.fold_left (fun acc pat -> (fp pat) :: acc) [] pl)
    | Tpat_or (pat1, pat2, _) ->
      List.flatten (List.fold_left (fun acc pat -> (fp pat) :: acc) [] [pat1; pat2])
    | Tpat_lazy pat ->
      fp pat
    | Tpat_constant _ ->
      []
    | Tpat_any ->
      []
    | Tpat_var (id, loc) ->
      [{
        ident_fname = "";
        ident_kind  = Def loc.Location.loc;
        ident_name  = Ident.name id;
        ident_loc   = loc.Location.loc;
      }]

(** iter_expression *)
and iter_expression f {exp_desc; exp_loc; exp_type; exp_extra; _} =
  let fe = iter_expression f in
  let fp = iter_pattern f in
  let fpe expr pe =
    List.iter begin fun (p, e) ->
      List.iter (fun d -> d.ident_kind <- Def expr.exp_loc; f d) (fp p);
      fe e;
    end pe
  in
  match exp_desc with
    | Texp_ident (id, loc, vd) ->
      let ident_kind = if Ident.name (Path.head id) = Path.last id then Int_ref vd.Types.val_loc else Ext_ref in
      let annot = {
        ident_fname = "";
        ident_kind;
        ident_name = Path.name id;
        ident_loc = loc.Location.loc
      } in
      f annot;
    | Texp_let (_, pe, expr) ->
      fpe expr pe;
      fe expr;
    | Texp_function (label, pe, _) ->
      List.iter begin fun (p, e) ->
        let defs =
          (List.map (fun d -> d.ident_kind <- Def e.exp_loc; d) (fp p)) @
          (List.map begin fun (pe, pe_loc) ->
            let def_loc =
              {pe_loc with loc_end = {pe_loc.loc_start with pos_cnum = pe_loc.loc_start.pos_cnum + String.length label}}
            in {
              ident_fname = "";
              ident_kind  = Def e.exp_loc;
              ident_name  = label;
              ident_loc   = def_loc
            }
          end p.pat_extra)
        in
        List.iter f defs;
        fe e;
      end pe
    | Texp_match (expr, pe, _) ->
      fe expr;
      List.iter begin fun (p, e) ->
        List.iter (fun d -> d.ident_kind <- Def e.exp_loc; f d) (fp p);
        fe e;
      end pe
    | Texp_apply (expr, pe) ->
      fe expr;
      List.iter (fun (_, e, _) -> Opt.may e fe) pe;
    | Texp_try (expr, pe) ->
      fe expr;
      List.iter begin fun (p, e) ->
        List.iter (fun d -> d.ident_kind <- Def e.exp_loc; f d) (fp p);
        fe e;
      end pe
    | Texp_tuple el -> List.iter fe el
    | Texp_construct (_, _, _, el, _) -> List.iter fe el
    | Texp_variant (_, expr) -> Opt.may expr fe
    | Texp_record (ll, expr) ->
      List.iter (fun (_, _, _, e) -> fe e) ll;
      Opt.may expr fe
    | Texp_field (expr, _, _, _) -> fe expr
    | Texp_setfield (e1, _, _, _, e2) -> fe e1; fe e2
    | Texp_array el -> List.iter fe el
    | Texp_ifthenelse (e1, e2, e3) -> fe e1; fe e2; Opt.may e3 fe
    | Texp_sequence (e1, e2) -> fe e1; fe e2
    | Texp_while (e1, e2) -> fe e1; fe e2
    | Texp_for (_, _, e1, e2, _, e3) -> fe e1; fe e2; fe e3
    | Texp_when (e1, e2) -> fe e1; fe e2
    | Texp_send (e1, _, e2) -> fe e1; Opt.may e2 fe
    | Texp_new (_, _, class_decl) -> ()
    | Texp_instvar _ -> ()
    | Texp_setinstvar (_, _, _, expr) -> fe expr
    | Texp_override (_, ll) -> List.iter (fun (_, _, e) -> fe e) ll
    | Texp_letmodule (_, _, mod_expr, expr) ->
      iter_module_expr f mod_expr;
      fe expr;
    | Texp_assert expr -> fe expr
    | Texp_assertfalse -> ()
    | Texp_lazy expr -> fe expr
    | Texp_object (cl_str, _) -> iter_class_structure f cl_str
    | Texp_pack mod_expr -> iter_module_expr f mod_expr
    | Texp_constant _ -> ()

(** iter_module_expr *)
and iter_module_expr f {mod_desc; mod_loc; _} =
  match mod_desc with
    | Tmod_structure {str_items; _} -> List.iter (iter_structure_item f) str_items
    | Tmod_functor (_, _, _, mod_expr) -> iter_module_expr f mod_expr
    | Tmod_apply (me1, me2, _) ->
      iter_module_expr f me1;
      iter_module_expr f me2;
    | Tmod_constraint (me, _, _, _) -> iter_module_expr f me;
    | Tmod_unpack (expr, _) -> ignore (iter_expression f expr)
    | Tmod_ident _ -> ();

(** iter_module_type *)
and iter_module_type f {mty_desc; mty_loc; _} =
  match mty_desc with
    | Tmty_ident _ -> ()
    | Tmty_signature sign -> List.iter (iter_signature_item f) sign.sig_items
    | Tmty_functor (_, _, mt1, mt2) ->
      iter_module_type f mt1;
      iter_module_type f mt2;
    | Tmty_with (mt, ll) ->
      iter_module_type f mt;
      List.iter (fun (_, loc, wc) -> iter_with_constraint f wc) ll
    | Tmty_typeof me -> iter_module_expr f me

(** iter_signature_item *)
and iter_signature_item f {sig_desc; sig_loc; _} =
  let fmt = iter_module_type f in
  match sig_desc with
    | Tsig_value (_, _, vd) -> iter_value_description f vd
    | Tsig_type ll -> List.iter (fun (_, _, td) -> iter_type_declaration f td) ll
    | Tsig_exception (_, _, ed) -> iter_exception_declaration f ed
    | Tsig_module (_, _, mt) -> iter_module_type f mt
    | Tsig_recmodule ll -> List.iter (fun (_, _, mt) -> fmt mt) ll
    | Tsig_modtype (_, _, Tmodtype_abstract) -> ()
    | Tsig_modtype (_, _, Tmodtype_manifest mt) -> fmt mt
    | Tsig_open _ -> ()
    | Tsig_include (mt, sign) -> fmt mt;
    | Tsig_class ll -> ()
    | Tsig_class_type ll -> ()

(** iter_value_description *)
and iter_value_description f {val_desc; val_loc; _} = iter_core_type f val_desc

(** iter_with_constraint *)
and iter_with_constraint f = function
  | Twith_type td -> iter_type_declaration f td
  | Twith_module _ -> ()
  | Twith_typesubst td -> iter_type_declaration f td
  | Twith_modsubst _ -> ()

(** iter_core_type *)
and iter_core_type f ?loc {ctyp_desc; ctyp_type; ctyp_loc; _} = ()

(** iter_class_field *)
and iter_class_field f {cf_desc; cf_loc} =
  match cf_desc with
    | Tcf_val (name, _, _, _, Tcfk_virtual core_type, _) -> iter_core_type f core_type
    | Tcf_val (name, _, _, _, Tcfk_concrete expr, _) -> ignore (iter_expression f expr)
    | Tcf_meth (_, loc, _, Tcfk_virtual core_type, _) -> iter_core_type f (*~loc:loc.loc*) core_type
    | Tcf_meth (_, loc, _, Tcfk_concrete expr, _) -> ignore (iter_expression f (*~loc:loc.loc*) expr)
    | Tcf_constr (ct1, ct2) ->
      iter_core_type f ct1;
      iter_core_type f ct2;
    | Tcf_init expr -> ignore (iter_expression f expr)
    | Tcf_inher (_, class_expr, _, _, _) -> iter_class_expr f class_expr

(** iter_class_structure *)
and iter_class_structure f {cstr_fields; _} = List.iter (iter_class_field f) cstr_fields

(** iter_class_expr *)
and iter_class_expr f {cl_desc; cl_loc; _} =
  match cl_desc with
    | Tcl_ident (_, _, core_type) -> List.iter (iter_core_type f) core_type
    | Tcl_structure str -> iter_class_structure f str
    | Tcl_fun (_, pat, ll, expr, _) ->
      let defs = iter_pattern f pat in
      List.iter (fun d -> d.ident_kind <- Def expr.cl_loc; f d) defs;
      (*List.iter (fun (_, _, expr) -> ignore (iter_expression f expr)) ll;*)
      iter_class_expr f expr
    | Tcl_apply (expr, ll) ->
      iter_class_expr f expr;
      List.iter (function (_, Some expr, _) -> ignore (iter_expression f expr) | _ -> ()) ll;
    | Tcl_let (_, ll, ll2, c_expr) ->
      List.iter begin fun (p, e) ->
        let defs = iter_pattern f p in
        List.iter (fun d -> d.ident_kind <- Def c_expr.cl_loc; f d) defs;
        iter_expression f e;
      end ll;
      (*List.iter begin fun (_, loc, expr) ->
        Printf.printf "----> %s\n%!" (string_of_loc loc.Location.loc);
        iter_expression f expr
      end ll2;*)
      iter_class_expr f c_expr;
    | Tcl_constraint (cle, clt, _, _, _) ->
      iter_class_expr f cle;
      Opt.may clt (iter_class_type f)

(** iter_class_type *)
and iter_class_type f {cltyp_desc; cltyp_loc; _} =
  match cltyp_desc with
    | Tcty_constr (_, _, ct) -> List.iter (iter_core_type f) ct
    | Tcty_signature sign -> iter_class_signature f sign
    | Tcty_fun (_, ct, clt) ->
      iter_core_type f ct;
      iter_class_type f clt

(** iter_class_type_field *)
and iter_class_type_field f {ctf_desc; ctf_loc; _} =
  match ctf_desc with
    | Tctf_inher clt -> iter_class_type f clt
    | Tctf_val (_, _, _, ct) -> iter_core_type f ct
    | Tctf_virt (_, _, ct) -> iter_core_type f ct
    | Tctf_meth (_, _, ct) -> iter_core_type f ct
    | Tctf_cstr (ct1, ct2) ->
      iter_core_type f ct1;
      iter_core_type f ct2;

(** iter_class_signature *)
and iter_class_signature f {csig_fields; _} = List.iter (iter_class_type_field f) csig_fields

(** iter_type_declaration *)
and iter_type_declaration f {typ_kind; typ_manifest; typ_cstrs; typ_loc; _} =
  begin
    match typ_kind with
      | Ttype_abstract -> ()
      | Ttype_variant ll ->
        List.iter begin fun (_, _, ct, _) ->
          List.iter (iter_core_type f) ct
        end ll
      | Ttype_record ll ->
        List.iter (fun (_, _, _, ct, _) -> iter_core_type f ct) ll
  end;
  List.iter begin fun (ct1, ct2, _) ->
    iter_core_type f ct1;
    iter_core_type f ct2;
  end typ_cstrs;
  Opt.may typ_manifest (iter_core_type f);

(** iter_exception_declaration *)
and iter_exception_declaration f {exn_params; exn_exn; exn_loc; _} = List.iter (iter_core_type f) exn_params

(** iter_structure_item *)
and iter_structure_item f {str_desc; str_loc; _} =
  match str_desc with
    | Tstr_eval expr -> ignore (iter_expression f expr);
    | Tstr_value (_, pe) ->
      let defs =
        List.rev (List.fold_left begin fun acc (pat, expr) ->
          let defs = iter_pattern f pat in
          ignore (iter_expression f expr);
          defs @ acc
        end [] pe)
      in
      List.iter (fun d -> d.ident_kind <- Def {str_loc with loc_start = str_loc.loc_end; loc_end = Lexing.dummy_pos}) defs;
      List.iter f defs
    | Tstr_open (_, lid) -> ()
    | Tstr_include (module_expr, _) -> ()
    | Tstr_class ll -> List.iter (fun (cd, _, _) -> iter_class_expr f cd.ci_expr) ll
    | Tstr_class_type ll -> List.iter (fun (_, _, cd) -> iter_class_type f cd.ci_expr) ll
    | Tstr_type ll -> List.iter (fun (_, _, td) -> iter_type_declaration f td) ll
    | Tstr_exception (_, _, ed) -> iter_exception_declaration f ed
    | Tstr_module (_, _, me) -> iter_module_expr f me
    | Tstr_modtype (_, _, mt) -> iter_module_type f mt
    | Tstr_recmodule ll ->
      List.iter begin fun (_, _, mt, me) ->
        iter_module_type f mt;
        iter_module_expr f me;
      end ll
    | Tstr_exn_rebind _ -> ()
    | Tstr_primitive (_, _, vd) -> iter_value_description f vd
;;

(** register *)
let register filename entry ({ident_loc; ident_kind; _} as annot) =
  if (*ident.[0] <> '*'*) ident_loc <> Location.none then begin
    annot.ident_fname <- filename;
    let start = ident_loc.loc_start.pos_cnum in
    let stop = ident_loc.loc_end.pos_cnum in
    for i = start to stop do entry.locations.(i) <- Some annot done;
    match ident_kind with
      | Int_ref def -> Hashtbl.add entry.int_refs def annot
      | Ext_ref ->
        begin
          match Longident.flatten (Longident.parse annot.ident_name) with
            | modname :: _ -> Hashtbl.add entry.ext_refs modname annot
            | _ -> ()
        end;
      | Def _ -> ()
  end

(** scan *)
let critical_scan = Mutex.create()

let scan ~project ~filename ?compile_buffer () =
  Mutex.lock critical_scan;
  try
    begin
      let timestamp = try (Hashtbl.find table_idents filename).timestamp with Not_found -> 0. in
      match Binannot.read_cmt ~project ~filename ~timestamp ?compile_buffer () with
        | Some (filename, timestamp, cmt) ->
          let size = (Unix.stat filename).Unix.st_size + 1 in
          let entry = {
            timestamp;
            locations = Array.create size None;
            int_refs  = Hashtbl.create 7;
            ext_refs  = Hashtbl.create 7
          } in
          Hashtbl.replace table_idents filename entry;
          let f = register filename entry in
          begin
            match cmt.cmt_annots with
              | Implementation {str_items; _} ->
                List.iter (iter_structure_item f) str_items;
              (*| Partial_implementation parts -> Array.iter (fold_part_impl f) parts*)
              | _ -> ()
          end
        | _ -> ()
    end;
    Mutex.unlock critical_scan;
  with ex -> begin
    Mutex.unlock critical_scan;
    raise ex;
  end
;;

(** find_external_definition *)
let find_external_definition ~project ~ident =
  let longid = Longident.parse ident.ident_name in
  match Longident.flatten longid with
    | name :: _ :: _ ->
      let name = name ^ ".ml" in
      let paths = Project.get_search_path_local project in
      begin
        try
          let filename = Misc.find_in_path_uncap paths name in
          scan ~project ~filename ();
          let {locations; _} = Hashtbl.find table_idents filename in
          let ext_name = Longident.last longid in
          begin
            try
              for i = Array.length locations - 1 downto 0 do
                match locations.(i) with
                  | None -> ()
                  | Some ({ident_kind; ident_name; _} as def_ident) ->
                    match ident_kind with
                      | Def scope when
                          scope.loc_end.pos_cnum = -1 &&
                          ident_name = ext_name ->
                            raise (Found def_ident)
                      | Int_ref _ | Ext_ref | Def _ -> ()
              done;
              assert false
            with Found def_ident -> Some (Project_def def_ident);
          end;
        with Not_found (* Misc.find_in_path_uncap *) -> Some Library_def
      end;
    | _ -> assert false
;;

(** scan_project_files *)
let scan_project_files ~project ?(sort=true) f =
  let src_files = File.readdirs ~links:false (Some (fun x -> x ^^ ".ml")) (project.Prj.root // Project.src) in
  let src_files = if sort then List.sort compare src_files else src_files in
  List.fold_left begin fun acc filename ->
    scan ~project ~filename ();
    try
      let entry = Hashtbl.find table_idents filename in
      f acc filename entry
    with Not_found -> acc
  end [] src_files
;;

(** find_project_external_references *)
let find_project_external_references ~project ~def =
  match def.ident_kind with
    | Def scope when scope.loc_end.pos_cnum = -1 ->
      let def_modname = modname_of_path def.ident_loc.loc_start.pos_fname in
      scan_project_files ~project begin fun acc filename entry ->
        try
          let mod_refs = Hashtbl.find_all entry.ext_refs def_modname in
          acc @ List.filter begin fun {ident_name; _} ->
            (Longident.last (Longident.parse ident_name)) = def.ident_name
          end mod_refs
        with Not_found -> acc
      end;
    | Int_ref _ | Ext_ref | Def _ -> []
;;

(** find_library_external_references *)
let find_library_external_references ~project ~ident:{ident_kind; ident_name; _} =
  match ident_kind with
    | Ext_ref ->
      scan_project_files ~project begin fun acc filename entry ->
        match Longident.flatten (Longident.parse ident_name) with
          | mod_name :: _ :: _ ->
            let all_refs = Hashtbl.find_all entry.ext_refs mod_name in
            let refs = List.filter (fun x -> x.ident_name = ident_name) all_refs in
            acc @ refs;
          | _ -> assert false
      end
    | Def _ | Int_ref _ -> []
;;

(** find_definition_and_references *)
let find_definition_and_references ~project ~entry ~ident =
  match ident.ident_kind with
    | Def _ -> (* Cursor offset is on a Def *)
      let int_refs = Hashtbl.find_all entry.int_refs ident.ident_loc in
      let ext_refs = find_project_external_references ~project ~def:ident in
      {bai_def = Some ident; bai_refs = int_refs @ ext_refs}
    | Int_ref def -> (* Cursor offset is on an Int_ref *)
      let bai_def = entry.locations.(def.loc_start.pos_cnum) in
      let int_refs = Hashtbl.find_all entry.int_refs def in
      let ext_refs = Opt.map_default bai_def [] (fun def -> find_project_external_references ~project ~def) in
      {bai_def; bai_refs = int_refs @ ext_refs}
    | Ext_ref -> (* Cursor offset is on an Ext_ref *)
      begin
        match find_external_definition ~project ~ident with
          | Some (Project_def def) ->
            let entry_def = try Some (Hashtbl.find table_idents def.ident_fname) with Not_found -> None in
            let int_refs = Opt.map_default entry_def [] (fun e -> Hashtbl.find_all e.int_refs def.ident_loc) in
            let ext_refs = find_project_external_references ~project ~def in
            {bai_def = Some def; bai_refs = int_refs @ ext_refs}
          | Some Library_def ->
            let refs = find_library_external_references ~project ~ident in
            {bai_def = None; bai_refs = refs}
          | None ->
            {bai_def = None; bai_refs = []}
      end
;;

(** find_ident *)
let find_ident ~project ~filename ~offset ?compile_buffer () =
  scan ~project ~filename ?compile_buffer ();
  try
    let {locations; _} as entry = Hashtbl.find table_idents filename in
    let ident = locations.(offset) in
    Opt.may ident begin fun ident ->
      let {bai_def; bai_refs} = find_definition_and_references ~project ~entry ~ident in
      Opt.may bai_def print_ident;
      List.iter print_ident bai_refs
    end;
  with Not_found -> Printf.eprintf "find_ident %s %d Not_found\n%!" filename offset;
;;



