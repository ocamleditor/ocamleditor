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
open Asttypes
open Location
open Lexing

let string_of_loc loc =
  let filename, a, b = Location.get_pos_info loc.loc_start in
  let _, c, d = Location.get_pos_info loc.loc_end in
  sprintf "%s, %d:%d(%d) -- %d:%d(%d)" filename a b (loc.loc_start.pos_cnum) c d (loc.loc_end.pos_cnum);;

let linechar_of_loc loc =
  let _, a, b = Location.get_pos_info loc.loc_start in
  let _, c, d = Location.get_pos_info loc.loc_end in
  ((a - 1), b), ((c - 1), d)

let string_of_type_expr te = Odoc_info.string_of_type_expr te;;

let (<==) loc offset = loc.loc_start.pos_cnum <= offset && offset <= loc.loc_end.pos_cnum

(** read *)
let read ~filename =
  let ext = if filename ^^ ".ml" then Some ".cmt" else if filename ^^ ".mli" then Some ".cmti" else None in
  match ext with
    | Some ext ->
      let filename_cmt = (Filename.chop_extension filename) ^ ext in
      Cmt_format.read filename_cmt
    | _ -> kprintf invalid_arg "Binannot.read \"%s\"" filename

(** find_pattern *)
let rec find_pattern f offset {pat_desc; pat_loc; pat_type; pat_extra; _} =
  if pat_loc <== offset then begin
    let fp = find_pattern f offset in
    let is_opt = ref false in
    let is_sth = ref false in
    begin
      match pat_desc with
        | Tpat_tuple pl -> List.iter fp pl
        | Tpat_alias (pat, _, _) -> fp pat
        | Tpat_construct (_, _, _, pl, _) -> List.iter fp pl
        | Tpat_variant (_, pat, _) -> Opt.may pat fp
        | Tpat_record (ll, _) -> List.iter (fun (_, _, _, pat) -> fp pat) ll
        | Tpat_array pl -> List.iter fp pl
        | Tpat_or (pat1, pat2, _) -> fp pat1; fp pat2
        | Tpat_lazy pat -> fp pat
        | Tpat_constant _ -> Printf.printf "Tpat_constant\n%!" ;
        | Tpat_any -> Printf.printf "Tpat_any\n%!" ;
        | Tpat_var (id, loc) ->
          is_opt := (Ident.name id) = "*opt*";
          is_sth := (Ident.name id) = "*sth*";
          Printf.printf "-->%s (pat_extra=%d) (loc=%s; %s)\n%!"
            (Ident.name id) (List.length pat_extra) (string_of_loc loc.loc) (loc.txt);
    end;
    (*List.iter (fun (pa, loc) -> if loc <== offset then Printf.printf "pat_extra\n%!" ;) pat_extra;*)
    Printf.printf "find_pattern: %s (pat_extra=%d)\n%!" (string_of_loc pat_loc) (List.length pat_extra);
    if not !is_opt then (f pat_loc (string_of_type_expr pat_type))
  end

(** find_expression *)
and find_expression f offset ?loc {exp_desc; exp_loc; exp_type; exp_extra; _} =
  let loc = Opt.default loc exp_loc in
  if loc <== offset then begin
    let fe = find_expression f offset in
    let fp = find_pattern f offset in
    begin
      match exp_desc with
        | Texp_ident (_, loc, vd) ->
          Printf.printf "==> %s %s\n%!" (Longident.last loc.txt) (string_of_loc loc.loc);
          (*f loc.loc (string_of_type_expr vd.Types.val_type)*)
        | Texp_let (_, pe, expr) ->
          List.iter begin fun (p, e) ->
            match p.pat_desc with
              | Tpat_var (id, _) when Ident.name id = "*opt*" -> fp p; fe e
              | _ -> fp p
          end pe;
          fe expr
        | Texp_function (lab, pe, _) ->
          Printf.printf "Texp_function %s (pe=%d) (exp_extra=%d)\n%!" lab (List.length pe) (List.length exp_extra);
          List.iter (fun (p, e) -> fp p; fe e) pe;
        | Texp_apply (e, ll) ->
          fe e;
          List.iter (fun (_, e, _) -> Opt.may e fe) ll
        | Texp_match (expr, ll, _) ->
          fe expr;
          List.iter (fun (p, e) -> fp p; fe e) ll;
        | Texp_try (expr, ll) ->
          fe expr;
          List.iter (fun (p, e) -> fp p; fe e) ll;
        | Texp_tuple ll -> List.iter fe ll
        | Texp_construct (_, _, _, ll, _) -> List.iter fe ll
        | Texp_variant (_, expr) -> Opt.may expr fe
        | Texp_record (ll, expr) ->
          List.iter (fun (_, _, _, e) -> fe e) ll;
          Opt.may expr fe
        | Texp_field (expr, _, _, _) -> fe expr
        | Texp_setfield (e1, _, _, _, e2) -> fe e1; fe e2
        | Texp_array ll -> List.iter fe ll
        | Texp_ifthenelse (e1, e2, e3) ->
          fe e1;
          fe e2;
          Opt.may e3 fe;
        | Texp_sequence (e1, e2) -> fe e1; fe e2
        | Texp_while (e1, e2) -> fe e1; fe e2
        | Texp_for (_, _, e1, e2, _, e3) -> fe e1; fe e2; fe e3
        | Texp_when (e1, e2) -> fe e1; fe e2
        | Texp_send (e1, _, e2) ->
          fe e1;
          Opt.may e2 fe
        | Texp_new (_, _, class_decl) -> Printf.printf "Texp_new\n%!" ;
        | Texp_instvar _ -> ()
        | Texp_setinstvar (_, _, _, expr) -> fe expr
        | Texp_override (_, ll) -> List.iter (fun (_, _, e) -> fe e) ll;
        | Texp_letmodule (_, _, mod_expr, expr) ->
          find_module_expr f offset mod_expr;
          fe expr
        | Texp_assert expr -> fe expr
        | Texp_assertfalse -> ()
        | Texp_lazy expr -> fe expr
        | Texp_object (cl_str, _) -> find_class_structure f offset cl_str
        | Texp_pack mod_expr -> find_module_expr f offset mod_expr
        | Texp_constant _ -> Printf.printf "Texp_constant\n%!" ;
    end;
    f loc (string_of_type_expr exp_type)
  end

(** find_module_expr *)
and find_module_expr f offset {mod_desc; mod_loc; _} =
  if mod_loc <== offset then begin
    begin
      match mod_desc with
        | Tmod_structure {str_items; _} -> List.iter (find_structure_item f offset) str_items
        | Tmod_functor (_, _, _, mod_expr) -> find_module_expr f offset mod_expr
        | Tmod_apply (me1, me2, _) ->
          find_module_expr f offset me1;
          find_module_expr f offset me2;
        | Tmod_constraint (me, _, _, _) -> find_module_expr f offset me;
        | Tmod_unpack (expr, _) -> find_expression f offset expr
        | Tmod_ident _ -> Printf.printf "*** Tmod_ident \n%!" ;
    end;
  end

(** find_module_type *)
and find_module_type f offset {mty_desc; mty_loc; _} =
  if mty_loc <== offset then begin
    match mty_desc with
      | Tmty_ident _ -> Printf.printf "Tmty_ident\n%!";
      | Tmty_signature sign -> List.iter (find_signature_item f offset) sign.sig_items
      | Tmty_functor (_, _, mt1, mt2) ->
        find_module_type f offset mt1;
        find_module_type f offset mt2;
      | Tmty_with (mt, ll) ->
        find_module_type f offset mt;
        List.iter (fun (_, loc, wc) -> if loc.loc <== offset then find_with_constraint f offset wc) ll
      | Tmty_typeof me -> find_module_expr f offset me
  end

(** find_signature_item *)
and find_signature_item f offset {sig_desc; sig_loc; _} =
  if sig_loc <== offset then begin
    let fmt = find_module_type f offset in
    match sig_desc with
      | Tsig_value (_, _, vd) -> find_value_description f offset vd
      | Tsig_type ll -> List.iter (fun (_, _, td) -> find_type_declaration f offset td) ll
      | Tsig_exception (_, _, ed) -> find_exception_declaration f offset ed
      | Tsig_module (_, _, mt) -> find_module_type f offset mt
      | Tsig_recmodule ll -> List.iter (fun (_, _, mt) -> fmt mt) ll
      | Tsig_modtype (_, _, Tmodtype_abstract) -> ()
      | Tsig_modtype (_, _, Tmodtype_manifest mt) -> fmt mt
      | Tsig_open _ -> ()
      | Tsig_include (mt, sign) -> fmt mt;
      | Tsig_class ll -> Printf.printf "Tsig_class\n%!";
      | Tsig_class_type ll -> Printf.printf "Tsig_class_type\n%!" ;
  end

(** find_value_description *)
and find_value_description f offset {val_desc; val_loc; _} =
  if val_loc <== offset then find_core_type f offset val_desc

(** find_with_constraint *)
and find_with_constraint f offset = function
  | Twith_type td -> find_type_declaration f offset td
  | Twith_module _ -> ()
  | Twith_typesubst td -> find_type_declaration f offset td
  | Twith_modsubst _ -> ()

(** find_core_type *)
and find_core_type f offset ?loc {ctyp_desc; ctyp_type; ctyp_loc; _} =
  let loc = Opt.default loc ctyp_loc in
  if loc <== offset then begin
    f loc (string_of_type_expr ctyp_type)
  end

(** find_class_field *)
and find_class_field f offset {cf_desc; cf_loc} =
  if cf_loc <== offset then begin
    match cf_desc with
      | Tcf_val (name, _, _, _, Tcfk_virtual core_type, _) -> find_core_type f offset core_type
      | Tcf_val (name, _, _, _, Tcfk_concrete expr, _) -> find_expression f offset expr
      | Tcf_meth (_, loc, _, Tcfk_virtual core_type, _) -> find_core_type f offset (*~loc:loc.loc*) core_type
      | Tcf_meth (_, loc, _, Tcfk_concrete expr, _) -> find_expression f (*~loc:loc.loc*) offset expr
      | Tcf_constr (ct1, ct2) ->
        find_core_type f offset ct1;
        find_core_type f offset ct2;
      | Tcf_init expr -> find_expression f offset expr
      | Tcf_inher (_, class_expr, _, _, _) -> find_class_expr f offset class_expr
  end

(** find_class_structure *)
and find_class_structure f offset {cstr_fields; _} =
  List.iter (find_class_field f offset) cstr_fields

(** find_class_expr *)
and find_class_expr f offset {cl_desc; cl_loc; _} =
  if cl_loc <== offset then begin
    match cl_desc with
      | Tcl_ident (_, _, core_type) -> List.iter (find_core_type f offset) core_type
      | Tcl_structure str -> find_class_structure f offset str
      | Tcl_fun (_, pat, ll, expr, _) ->
        find_pattern f offset pat;
        List.iter (fun (_, _, expr) -> find_expression f offset expr) ll;
        find_class_expr f offset expr
      | Tcl_apply (expr, ll) ->
        find_class_expr f offset expr;
        List.iter (function (_, Some expr, _) -> find_expression f offset expr | _ -> ()) ll;
      | Tcl_let (_, ll, ll2, expr) ->
        List.iter begin fun (pat, expr) ->
          find_pattern f offset pat;
          find_expression f offset expr;
        end ll;
        List.iter (fun (_, _, expr) -> find_expression f offset expr) ll2;
        find_class_expr f offset expr;
      | Tcl_constraint (cle, clt, _, _, _) ->
        find_class_expr f offset cle;
        Opt.may clt (find_class_type f offset)
  end

(** find_class_type *)
and find_class_type f offset {cltyp_desc; cltyp_loc; _} =
  if cltyp_loc <== offset then begin
    match cltyp_desc with
      | Tcty_constr (_, _, ct) -> List.iter (find_core_type f offset) ct
      | Tcty_signature sign -> find_class_signature f offset sign
      | Tcty_fun (_, ct, clt) ->
        find_core_type f offset ct;
        find_class_type f offset clt
  end

(** find_class_type_field *)
and find_class_type_field f offset {ctf_desc; ctf_loc; _} =
  if ctf_loc <== offset then begin
    match ctf_desc with
      | Tctf_inher clt -> find_class_type f offset clt
      | Tctf_val (_, _, _, ct) -> find_core_type f offset ct
      | Tctf_virt (_, _, ct) -> find_core_type f offset ct
      | Tctf_meth (_, _, ct) -> find_core_type f offset ct
      | Tctf_cstr (ct1, ct2) ->
        find_core_type f offset ct1;
        find_core_type f offset ct2;
  end

(** find_class_signature *)
and find_class_signature f offset {csig_fields; _} =
  List.iter (find_class_type_field f offset) csig_fields

(** find_type_declaration *)
and find_type_declaration f offset {typ_kind; typ_manifest; typ_cstrs; typ_loc; _} =
  if typ_loc <== offset then begin
    begin
      match typ_kind with
        | Ttype_abstract -> ()
        | Ttype_variant ll ->
          List.iter begin fun (_, _, ct, _) ->
            List.iter (find_core_type f offset) ct
          end ll
        | Ttype_record ll ->
          List.iter (fun (_, _, _, ct, _) -> find_core_type f offset ct) ll
    end;
    List.iter begin fun (ct1, ct2, _) ->
      find_core_type f offset ct1;
      find_core_type f offset ct2;
    end typ_cstrs;
    Opt.may typ_manifest (find_core_type f offset);
  end

(** find_exception_declaration *)
and find_exception_declaration f offset {exn_params; exn_exn; exn_loc; _} =
  if exn_loc <== offset then begin
    List.iter (find_core_type f offset) exn_params
  end

(** find_structure_item *)
and find_structure_item f offset {str_desc; str_loc; _} =
  if str_loc <== offset then
    match str_desc with
      | Tstr_eval expr -> find_expression f offset expr;
      | Tstr_value (_, pe) ->
        List.iter begin fun (pat, expr) ->
          find_pattern f offset pat;
          find_expression f offset expr;
        end pe
      | Tstr_open (_, lid) -> f str_loc (String.concat "." (Longident.flatten lid.txt))
      | Tstr_include (module_expr, _) -> find_module_expr f offset module_expr
      | Tstr_class ll -> List.iter (fun (cd, _, _) -> find_class_expr f offset cd.ci_expr) ll
      | Tstr_class_type ll -> List.iter (fun (_, _, cd) -> find_class_type f offset cd.ci_expr) ll
      | Tstr_type ll -> List.iter (fun (_, _, td) -> find_type_declaration f offset td) ll
      | Tstr_exception (_, _, ed) -> find_exception_declaration f offset ed
      | Tstr_module (_, _, me) -> find_module_expr f offset me
      | Tstr_modtype (_, _, mt) -> find_module_type f offset mt
      | Tstr_recmodule ll ->
        List.iter begin fun (_, _, mt, me) ->
          find_module_type f offset mt;
          find_module_expr f offset me;
        end ll
      | Tstr_exn_rebind _ -> Printf.printf "Tstr_exn_rebind\n%!" ;
      | Tstr_primitive (_, _, vd) -> find_value_description f offset vd
;;

(** get_annot *)
let get_annot ~filename ~offset =
  let cmi, cmt = read ~filename in
  let f loc txt =
    Printf.printf "--------> %d; %s : %s\n%!" offset (string_of_loc loc) txt;
    raise Exit;
  in
  try
    Opt.may cmt begin fun cmt ->
      Odoc_info.reset_type_names();
      match cmt.cmt_annots with
        | Implementation {str_items; _} -> List.iter (find_structure_item f offset) str_items
        | _ -> ()
    end
  with Exit -> ()
