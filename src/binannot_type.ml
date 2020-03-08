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


open Miscellanea
open Cmt_format
open Typedtree
open Location
open Binannot

type t = {
  ba_loc  : Location.t;
  ba_type : string;
}

exception Found of t

module Log = Common.Log.Make(struct let prefix = "Binannot_type" end)
let _ = Log.set_verbosity `ERROR

let arg_label_to_string = function
  | Asttypes.Nolabel -> ""
  | Asttypes.Labelled s -> s
  | Asttypes.Optional s -> s

(** find_pattern *)
let rec find_pattern f offset ?(opt=false, false) {pat_desc; pat_loc; pat_type; pat_extra; _} =
  if pat_loc <== offset then begin
    let fp = find_pattern f offset in
    let (opt, sth) as result =
      match pat_desc with
        | Tpat_tuple pl ->
          Log.println `DEBUG "Tpat_tuple";
          List.fold_left (fun opt pat -> fp ~opt pat) opt pl
        | Tpat_alias (pat, _, _) ->
          Log.println `DEBUG "Tpat_alias" ;
          fp ~opt pat
        | Tpat_construct ({ Asttypes.txt; loc }, _, pl) ->
          Log.println `DEBUG "Tpat_construct (%s) (%s) (%d)" (Longident.last txt) (string_of_loc loc) (List.length pl);
          List.fold_left (fun opt pat -> fp ~opt pat) opt pl
        | Tpat_variant (lab, pat, _) ->
          Log.println `DEBUG "Tpat_variant (%s)" lab;
          Opt.map_default pat opt (fun pat -> fp ~opt pat)
        | Tpat_record (ll, _) ->
          Log.println `DEBUG "Tpat_record ";
          List.fold_left (fun opt (_, _, pat) -> fp ~opt pat) opt ll
        | Tpat_array pl ->
          Log.println `DEBUG "Tpat_array ";
          List.fold_left (fun opt pat -> fp ~opt pat) opt pl
        | Tpat_or (pat1, pat2, _) ->
          Log.println `DEBUG "Tpat_or ";
          List.fold_left (fun opt pat -> fp ~opt pat) opt [pat1; pat2]
        | Tpat_lazy pat ->
          Log.println `DEBUG "Tpat_lazy ";
          fp ~opt pat
        | Tpat_constant _ ->
          Log.println `DEBUG "Tpat_constant" ;
          opt
        | Tpat_any ->
          Log.println `DEBUG "Tpat_any" ;
          opt
        | Tpat_var (id, { Asttypes.txt; loc  }) ->
          Log.println `DEBUG "Tpat_var: %s (pat_extra=%d) (loc=%s; %s)"
            (Ident.name id) (List.length pat_extra) (string_of_loc loc) txt;
          (Ident.name id) = "*opt*", (Ident.name id) = "*sth*"
        (* From 4.08 TODO: *)
        | Tpat_exception _ ->
          Log.println `DEBUG "Tpat_exception";
          opt
    in
    if not opt && not sth then begin
       Log.println `DEBUG "find_pattern: %s (pat_extra=%d) (%b,%b)"
         (string_of_loc pat_loc) (List.length pat_extra) opt sth;
       f pat_loc (string_of_type_expr pat_type)
    end;
    result
  end else opt

(** find_expression *)
and find_expression f offset ?(opt=false,false) ?loc {exp_desc; exp_loc; exp_type; exp_extra; _} =
  let loc = Opt.default loc exp_loc in
  if loc <== offset then begin
    let (opt, sth) as result =
      let fe = find_expression f offset in
      let fp = find_pattern f offset in
      match exp_desc with
        | Texp_ident (id, { Asttypes.txt; loc }, _) ->
          Log.println `DEBUG "Texp_ident: %s %s (%s)" (Longident.last txt) (string_of_loc loc) (Path.name id);
          Path.name id = "*opt*", Path.name id = "*sth*"
        | Texp_let (_, pe, expr) ->
          Log.println `DEBUG "Texp_let " ;
          ignore (List.fold_left begin fun opt { vb_pat = p; vb_expr = e; _ } ->
            fp p |> ignore;
            fe ~opt e
          end opt pe);
          fe ~opt:(false, false) expr
        (* Added in 4.04 *)
        | Texp_letexception (extension_constructor, expr) ->
           find_extension_constructor f offset extension_constructor;
           fe ~opt expr
        | Texp_function { arg_label; cases; _ } ->
          Log.println `DEBUG "Texp_function: %s (pe=%d) (exp_extra=%d) (%s)"
            (arg_label_to_string arg_label) (List.length cases) (List.length exp_extra) (string_of_loc exp_loc);
          List.fold_left begin fun opt { c_lhs = p; c_guard = oe; c_rhs = e } ->
            fp p |> ignore;
            let opt = match oe with Some e' -> fe ~opt e' | None -> opt in
            fe ~opt e;
          end opt cases;
        | Texp_match (expr, cl, _) ->
          Log.println `DEBUG "Texp_match: " ;
          let opt = fe ~opt expr in
          let o1, _ = opt in
          let opt =
            List.fold_left begin fun opt { c_lhs = p; c_guard = oe; c_rhs = e } ->
              if not o1 then (ignore (fp p));
              let opt = match oe with Some e' -> fe ~opt e' | None -> opt in
              fe ~opt e;
            end opt cl
          in
          opt
        | Texp_apply (e, ll) ->
          Log.println `DEBUG "Texp_apply: " ;
          let opt = fe ~opt e in
          List.fold_left (fun opt (_, e) -> Opt.map_default e opt (fun e -> fe ~opt e)) opt ll
        | Texp_try (expr, ll) ->
          let opt = fe expr in
          List.fold_left begin fun opt { c_lhs = p; c_guard = oe; c_rhs = e } ->
            fp p |> ignore;
            let opt = match oe with Some e' -> fe ~opt e' | None -> opt in
            fe ~opt e
          end opt ll;
        | Texp_tuple ll ->
          Log.println `DEBUG "Texp_tuple: " ;
          List.fold_left (fun opt e -> fe ~opt e) opt ll
        | Texp_construct (_, _, ll) ->
          Log.println `DEBUG "Texp_construct: " ;
          List.fold_left (fun opt e -> fe ~opt e) opt ll
        | Texp_variant (_, expr) ->
          Opt.map_default expr opt (fun e -> fe ~opt e)
        | Texp_record { fields; extended_expression; _ } ->
          let fold_field opt (_, f)  = match f with
            | Kept _ -> opt
            | Overridden (_, e) -> fe ~opt e
          in
          let opt = Array.fold_left fold_field opt fields in
          Opt.map_default extended_expression opt (fun e -> fe ~opt e)
        | Texp_field (expr, _, _) ->
          fe ~opt expr
        | Texp_setfield (e1, _, _, e2) ->
          let opt = fe ~opt e1 in
          fe ~opt e2
        | Texp_array ll ->
          List.fold_left (fun opt e -> fe ~opt e) opt ll
        | Texp_ifthenelse (e1, e2, e3) ->
          let opt = fe ~opt e1 in
          let opt = fe ~opt e2 in
          Opt.map_default e3 opt (fun e -> fe ~opt e)
        | Texp_sequence (e1, e2) ->
          let opt = fe ~opt e1 in
          fe ~opt e2
        | Texp_while (e1, e2) ->
          let opt = fe ~opt e1 in
          fe ~opt e2
        | Texp_for (_, _, e1, e2, _, e3) ->
          let opt = fe ~opt e1 in
          let opt = fe ~opt e2 in
          fe ~opt e3
        | Texp_send (e1, _, e2) ->
          let opt = fe e1 in
          Opt.map_default e2 opt (fun e -> fe ~opt e)
        | Texp_new (_, _, _) ->
          Log.println `DEBUG "Texp_new" ;
          opt
        | Texp_instvar _ -> opt
        | Texp_setinstvar (_, _, _, expr) ->
          fe ~opt expr
        | Texp_override (_, ll) ->
          List.fold_left (fun opt (_, _, e) -> fe ~opt e) opt ll;
        | Texp_letmodule (_, _, _, mod_expr, expr) ->
          find_module_expr f offset mod_expr;
          fe ~opt expr
        | Texp_assert expr ->
          fe ~opt expr
        | Texp_lazy expr ->
          fe ~opt expr
        | Texp_object (cl_str, _) ->
          find_class_structure f offset cl_str;
          opt
        | Texp_pack mod_expr ->
          find_module_expr f offset mod_expr;
          opt
        | Texp_constant _ ->
          Log.println `DEBUG "Texp_constant" ;
          opt
        (* Since 4.03 *)
        | Texp_unreachable ->
          Log.println `DEBUG "Texp_unreachable" ;
          opt
        | Texp_extension_constructor ({ Asttypes.txt; loc }, id) ->
          Log.println `DEBUG "Texp_extension_constructor: %s %s (%s)" (Longident.last txt) (string_of_loc loc) (Path.name id);
          Path.name id = "*opt*", Path.name id = "*sth*"
        (* Since 4.08 TODO: *)
        | Texp_letop _ ->
          Log.println `DEBUG "Texp_letop";
          opt;
        | Texp_open _ ->
          Log.println `DEBUG "Texp_open";
          opt
    in
    if not opt && not sth then begin
       Log.println `DEBUG "find_expression: %s (%b,%b)" (string_of_loc exp_loc) opt sth;
      f loc (string_of_type_expr exp_type);
      result
    end else if sth then opt, false else result
  end else opt

(** find_module_expr *)
and find_module_expr f offset {mod_desc; mod_loc; _} =
  if mod_loc <== offset then begin
    begin
      match mod_desc with
        | Tmod_structure {str_items; _} -> List.iter (find_structure_item f offset) str_items
        | Tmod_functor ( _, mod_expr) -> find_module_expr f offset mod_expr
        | Tmod_apply (me1, me2, _) ->
          find_module_expr f offset me1;
          find_module_expr f offset me2;
        | Tmod_constraint (me, _, _, _) -> find_module_expr f offset me;
        | Tmod_unpack (expr, _) -> ignore (find_expression f offset expr)
        | Tmod_ident _ -> Log.println `DEBUG "*** Tmod_ident " ;
    end;
  end

(** find_module_type *)
and find_module_type f offset {mty_desc; mty_loc; _} =
  if mty_loc <== offset then begin
    match mty_desc with
      | Tmty_ident _ -> Log.println `DEBUG "Tmty_ident";
      | Tmty_alias _ -> Log.println `DEBUG "Tmty_alias";
      | Tmty_signature sign -> List.iter (find_signature_item f offset) sign.sig_items
      | Tmty_functor (Unit, mt) ->
                      find_module_type f offset mt;
      | Tmty_functor (Named (_, _, mt1), mt2) ->
                      find_module_type f offset mt1;
                      find_module_type f offset mt2
      | Tmty_with (mt, ll) ->
        find_module_type f offset mt;
        List.iter (fun (_, { Asttypes.loc; _ }, wc) -> if loc <== offset then find_with_constraint f offset wc) ll
      | Tmty_typeof me -> find_module_expr f offset me
  end

(** find_signature_item *)
and find_signature_item f offset {sig_desc; sig_loc; _} =
  if sig_loc <== offset then begin
    let fmt = find_module_type f offset in
    match sig_desc with
      | Tsig_value vdesc -> find_value_description f offset vdesc
      | Tsig_type (_, ll) -> List.iter (fun td -> find_type_declaration f offset td) ll
      | Tsig_exception { tyexn_constructor; _ } -> find_extension_constructor f offset tyexn_constructor
      | Tsig_module mdecl -> find_module_type f offset mdecl.md_type
      | Tsig_recmodule ll -> List.iter (fun mdecl -> fmt mdecl.md_type) ll
      | Tsig_modtype mtdecl -> Opt.may mtdecl.mtd_type fmt
      | Tsig_open _ -> ()
      | Tsig_include idecl -> fmt idecl.incl_mod;
      | Tsig_class _ -> Log.println `DEBUG "Tsig_class";
      | Tsig_class_type _ -> Log.println `DEBUG "Tsig_class_type"
      | Tsig_typext _ -> Log.println `DEBUG "Tsig_typext"
      | Tsig_attribute _ -> Log.println `DEBUG "Tsig_attribute"
      | Tsig_typesubst _ -> Log.println `DEBUG "Tsig_typesubst"
      | Tsig_modsubst _  -> Log.println `DEBUG "Tsig_modsubst"
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
and find_core_type f offset ?loc {ctyp_type; ctyp_loc; _} =
  let loc = Opt.default loc ctyp_loc in
  if loc <== offset then begin
    f loc (string_of_type_expr ctyp_type)
  end

(** find_class_field *)
and find_class_field f offset {cf_desc; cf_loc; _} =
  if cf_loc <== offset then begin
    match cf_desc with
      | Tcf_val (_, _, _, Tcfk_virtual core_type, _) -> find_core_type f offset core_type
      | Tcf_val (_, _, _, Tcfk_concrete (_, expr), _) -> ignore (find_expression f offset expr)
      | Tcf_method (_, _, Tcfk_virtual core_type) -> find_core_type f offset (*~loc:loc.loc*) core_type
      | Tcf_method (_, _, Tcfk_concrete (_, expr)) -> ignore (find_expression f (*~loc:loc.loc*) offset expr)
      | Tcf_constraint (ct1, ct2) ->
        find_core_type f offset ct1;
        find_core_type f offset ct2;
      | Tcf_initializer expr -> ignore (find_expression f offset expr)
      | Tcf_inherit (_, class_expr, _, _, _) -> find_class_expr f offset class_expr
      (* Added in 4.02.0 - TODO *)
      | Tcf_attribute _ -> ()
  end

(** find_class_structure *)
and find_class_structure f offset {cstr_fields; _} =
  List.iter (find_class_field f offset) cstr_fields

(** find_class_expr *)
and find_class_expr f offset {cl_desc; cl_loc; _} =
  if cl_loc <== offset then begin
    match cl_desc with
      | Tcl_ident (_, _, core_type) -> List.iter (find_core_type f offset ?loc:None) core_type
      | Tcl_structure str -> find_class_structure f offset str
      | Tcl_fun (_, pat, ll, expr, _) ->
        find_pattern f offset pat |> ignore;
        List.iter (fun (_, expr) -> ignore (find_expression f offset expr)) ll;
        find_class_expr f offset expr
      | Tcl_apply (expr, ll) ->
        find_class_expr f offset expr;
        List.iter (function (_, Some expr) -> ignore (find_expression f offset expr) | _ -> ()) ll;
      | Tcl_let (_, ll, ll2, expr) ->
        List.iter begin fun { vb_pat = pat; vb_expr = expr; _ } ->
          find_pattern f offset pat |> ignore;
          find_expression f offset expr |> ignore;
        end ll;
        List.iter (fun (_, expr) -> ignore (find_expression f offset expr)) ll2;
        find_class_expr f offset expr;
      | Tcl_constraint (cle, clt, _, _, _) ->
        find_class_expr f offset cle;
        Opt.may clt (find_class_type f offset)
      (* Added in 4.06 *)
      | Tcl_open (_, class_expr) ->
        find_class_expr f offset class_expr
  end

(** find_class_type *)
and find_class_type f offset {cltyp_desc; cltyp_loc; _} =
  if cltyp_loc <== offset then begin
    match cltyp_desc with
      | Tcty_constr (_, _, ct) -> List.iter (find_core_type f offset ?loc:None) ct
      | Tcty_signature sign -> find_class_signature f offset sign
      | Tcty_arrow (_, ct, clt) ->
        find_core_type f offset ct;
        find_class_type f offset clt
      (* Added in 4.06 *)
      | Tcty_open (_, class_type) ->
        find_class_type f offset class_type
  end

(** find_class_type_field *)
and find_class_type_field f offset {ctf_desc; ctf_loc; _} =
  if ctf_loc <== offset then begin
    match ctf_desc with
      | Tctf_inherit clt -> find_class_type f offset clt
      | Tctf_val (_, _, _, ct) -> find_core_type f offset ct
      | Tctf_method (_, _, _, ct) -> find_core_type f offset ct
      | Tctf_constraint (ct1, ct2) ->
        find_core_type f offset ct1;
        find_core_type f offset ct2;
      (* Added in 4.02.0 - TODO *)
      | Tctf_attribute _ -> ()
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
         List.iter begin fun { cd_args; _ } ->
            match cd_args with
            | Cstr_tuple ct -> List.iter (find_core_type ?loc:None f offset) ct
            | Cstr_record ll -> List.iter (fun { ld_type = ct; _ } -> find_core_type f offset ct) ll
          end ll
        | Ttype_record ll ->
          List.iter (fun { ld_type = ct; _ } -> find_core_type f offset ct) ll
        (* Added in 4.02.0 *)
        | Ttype_open -> ()
    end;
    List.iter begin fun (ct1, ct2, _) ->
      find_core_type f offset ct1;
      find_core_type f offset ct2;
    end typ_cstrs;
    Opt.may typ_manifest (find_core_type f offset ?loc:None);
  end

and find_extension_constructor f offset { ext_loc; ext_kind; _ } =
  if ext_loc <== offset then begin
    match ext_kind with
    | Text_decl (arguments, core_type_option) ->
      begin match arguments with
        | Cstr_tuple core_types ->
          List.iter (find_core_type f offset ?loc:None) core_types;
        | Cstr_record label_declarations ->
          List.iter (fun ld -> find_core_type f offset ld.ld_type) label_declarations
      end;
      Opt.may core_type_option (find_core_type f offset ?loc:None)
        (* TODO *)
    | Text_rebind (_, _) -> ()
  end

(** find_structure_item *)
and find_structure_item f offset {str_desc; str_loc; _} =
  if str_loc <== offset then
    match str_desc with
      | Tstr_eval (expr, _) -> ignore (find_expression f offset expr);
      | Tstr_value (_, pe) ->
        List.iter begin fun { vb_pat = pat; vb_expr = expr; _ } ->
          find_pattern f offset pat |> ignore;
          ignore (find_expression f offset expr);
        end pe
      (*| Tstr_open odesc -> f str_loc (Odoc_misc.string_of_longident odesc.open_txt.txt)*)
      (* Changed in 4.08 TODO: *)
      | Tstr_open _odecl -> ()
      | Tstr_include idesc -> find_module_expr f offset idesc.incl_mod
      | Tstr_class ll -> List.iter (fun (cd, _) -> find_class_expr f offset cd.ci_expr) ll
      | Tstr_class_type ll -> List.iter (fun (_, _, cd) -> find_class_type f offset cd.ci_expr) ll
      | Tstr_type (_, ll) -> List.iter (fun td -> find_type_declaration f offset td) ll
      | Tstr_exception { tyexn_constructor; _ } -> find_extension_constructor f offset tyexn_constructor
      | Tstr_module mdecl -> find_module_expr f offset mdecl.mb_expr
      | Tstr_modtype mtdecl -> Opt.may mtdecl.mtd_type (find_module_type f offset)
      | Tstr_recmodule ll ->
        List.iter begin fun { mb_expr = me; _ } ->
          (* Removed in 4.02.0 *)
          (*find_module_type f offset mt;*)
          find_module_expr f offset me;
        end ll
      (* Removed in 4.02.0 *)
      (*| Tstr_exn_rebind _ -> Log.println `DEBUG "Tstr_exn_rebind" ;*)
      | Tstr_primitive vdecl -> find_value_description f offset vdecl
      (* Added in 4.02.0 *)
      | Tstr_attribute _ -> Log.println `DEBUG "Tstr_attribute"
      | Tstr_typext { tyext_params; tyext_constructors; _ } ->
        List.iter (fun (ct, _) -> find_core_type f offset ct) tyext_params;
        List.iter (find_extension_constructor f offset) tyext_constructors
;;

(** find_part_impl *)
let find_part_impl f offset = function
  | Partial_structure {str_items; _} -> List.iter (find_structure_item f offset) str_items
  | Partial_structure_item item -> find_structure_item f offset item
  | Partial_expression expr -> ignore (find_expression f offset expr)
  | Partial_pattern pat -> ignore (find_pattern f offset pat)
  | Partial_class_expr cl_expr -> ignore (find_class_expr f offset cl_expr)
  | Partial_signature sigs ->List.iter (find_signature_item f offset) sigs.sig_items
  | Partial_signature_item sig_item -> find_signature_item f offset sig_item
  | Partial_module_type mtyp -> find_module_type f offset mtyp;;

(** find_by_offset *)
let find_by_offset ~project ~filename ~offset ?compile_buffer () =
  match Binannot.read_cmt ~project ~filename ?compile_buffer () with
    | Some (_, _, cmt) ->
      begin
        let f ba_loc ba_type =
          Log.println `TRACE "%d; %s : %s" offset (string_of_loc ba_loc) ba_type;
          raise (Found {ba_loc; ba_type});
        in
        try
          Odoc_info.reset_type_names();
          begin
            match [@warning "-4"] cmt.cmt_annots with
              | Implementation {str_items; _} -> List.iter (find_structure_item f offset) str_items
              | Partial_implementation parts -> Array.iter (find_part_impl f offset) parts
              | _ -> ()
          end;
          None
        with Found ba -> Some ba
      end;
    | _ -> None

(** find *)
let find ~page ?iter () =
  let compile_buffer () = page#compile_buffer ?join:(Some true) () in
  let iter = match iter with Some it -> it | _ -> page#buffer#get_iter `INSERT in
  find_by_offset ~project:page#project ~filename:page#get_filename ~offset:iter#offset ~compile_buffer ()
(*  match Binannot.read_cmt ~project:page#project ~filename:page#get_filename ~compile_buffer () with
    | Some (filename, _, cmt) ->
      begin
        let offset = iter#offset in
        let f ba_loc ba_type =
          Log.println `TRACE "%d; %s : %s" offset (string_of_loc ba_loc) ba_type;
          raise (Found {ba_loc; ba_type});
        in
        try
          Odoc_info.reset_type_names();
          begin
            match cmt.cmt_annots with
              | Implementation {str_items; _} -> List.iter (find_structure_item f offset) str_items
              | Partial_implementation parts -> Array.iter (find_part_impl f offset) parts
              | _ -> ()
          end;
          None
        with Found ba -> Some ba
      end;
    | _ -> None*)
