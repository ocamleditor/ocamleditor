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
open Lexing
open Binannot

module Log = Common.Log.Make(struct let prefix = "Binannot_ident" end)
let _ = Log.set_verbosity `TRACE

exception Found of ident

type ext_def = Project_def of ident | Project_file of ident | Library_def | No_def

(** iter_pattern *)
let rec iter_pattern f {pat_desc; pat_loc; _} =
  let fp = iter_pattern f in
  match pat_desc with
    | Tpat_tuple pl ->
      List.flatten (List.fold_left (fun acc pat -> (fp pat) :: acc) [] pl)
    | Tpat_alias (pat, id, loc) ->
      let name_loc = pat_loc(*loc.loc*) in
      {
        ident_fname      = "";
        ident_kind       = Def {def_name=loc.txt; def_loc=name_loc; def_scope=none};
        ident_loc        = Location.mkloc (Ident.name id) name_loc;
      } :: (fp pat)
    | Tpat_construct ({ Asttypes.loc; _ }, { Types.cstr_name; cstr_res; cstr_tag; _ }, pl) ->
      let type_expr = lid_of_type_expr cstr_res in
      let path, is_qualified =
        match cstr_tag with
          | Types.Cstr_extension (p, _) ->
            let path = Longident.parse (Path.name p) in
            path, Longident.qualified path
          | Types.Cstr_constant _
          | Types.Cstr_block _
          | Types.Cstr_unboxed ->
            Longident.of_type_expr type_expr cstr_name,
            Longident.qualified type_expr
      in
      let ident_kind = if is_qualified then Ext_ref else Int_ref none in
      let ident = {
        ident_kind;
        ident_fname = "";
        ident_loc   = Location.mkloc (Odoc_misc.string_of_longident path) loc;
      } in
      f ident;
      List.flatten (List.fold_left (fun acc pat -> (fp pat) :: acc) [] pl)
    | Tpat_variant (_, pat, _) ->
      Opt.map_default pat [] (fun pat -> fp pat)
    | Tpat_record (ll, _) ->
      List.flatten (List.fold_left begin fun acc ({ Asttypes.loc; _ }, ld, pat) ->
        let type_expr = lid_of_type_expr ld.Types.lbl_res in
        let ident_kind = if Longident.qualified type_expr then Ext_ref else Int_ref none in
        let path = Longident.of_type_expr type_expr ld.Types.lbl_name in
        let ident = {
          ident_kind;
          ident_fname = "";
          ident_loc   = Location.mkloc (Odoc_misc.string_of_longident path) loc;
        } in
        f ident;
        (fp pat) :: acc
      end [] ll)
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
    | Tpat_var (id, { Asttypes.txt; loc }) ->
      [{
        ident_fname      = "";
        ident_kind       = Def { def_name=txt; def_loc=loc; def_scope=none };
        ident_loc        = Location.mkloc (Ident.name id) loc;
      }]
    (* Since 4.08 *)
    | Tpat_exception pat -> fp pat

(** iter_expression *)
and iter_expression f {exp_desc; exp_extra; _} =
  let fe = iter_expression f in
  let fp = iter_pattern f in
  let fvb expr vbl =
    List.iter begin fun { vb_pat; vb_expr; _ } ->
      List.iter begin function [@warning "-4"]
        | {ident_kind = Def d; _} as i ->
          d.def_scope <- expr.exp_loc;
          f i
        | i -> f i
      end (fp vb_pat);
      fe vb_expr;
    end vbl
  in
  let arg_label_to_string arg_label =
    let open Asttypes in
    match arg_label with
    | Nolabel -> ""
    | Labelled s -> s
    | Optional s -> s
  in
  List.iter begin fun (exp, loc', _) ->
    match exp with
      (* Remomed in 4.08 *)
      (*| Texp_open (_, path, { Asttypes.loc; _ }, _) ->
        f {
          ident_kind  = Open loc';
          ident_fname = "";
          ident_loc   = Location.mkloc (Path.name path) loc;
        };*)
      | Texp_constraint _
      | Texp_coerce _
      | Texp_poly _
      | Texp_newtype _ -> ()
  end exp_extra;
  match exp_desc with
    | Texp_ident (id, { Asttypes.loc; _ }, vd) ->
      let ident_kind =
        if Ident.name (Path.head id) = Path.last id
        then Int_ref (*none*) vd.Types.val_loc (* In caso di alias val_loc si riferisce all'intero binding e non solo al nome *)
        else Ext_ref
      in
      let ident = {
        ident_fname      = "";
        ident_kind;
        ident_loc        = Location.mkloc (Path.name id) loc;
      } in
      f ident;
    | Texp_let (_, vbl, expr) ->
      fvb expr vbl;
      fe expr;
    | Texp_letexception (extension_constructor, expr) ->
      iter_extension_constructor f extension_constructor;
      fe expr
    | Texp_function { arg_label; cases; _ } ->
      List.iter
        begin fun { c_lhs; c_guard; c_rhs } ->
           let defs =
             (List.map begin function [@warning "-4"]
                                    | {ident_kind=Def d; _} as i ->
                 d.def_scope <- c_rhs.exp_loc;
                 i
                                    | i -> i
               end (fp c_lhs)) @
             (List.map begin fun (_, pe_loc, _) ->
                 let label = arg_label_to_string arg_label in
                 let def_loc =
                   {pe_loc with loc_end = {pe_loc.loc_start with pos_cnum = pe_loc.loc_start.pos_cnum + String.length label}}
                 in {
                   ident_fname      = "";
                   ident_kind       = Def {def_name=""; def_loc=none; def_scope=c_rhs.exp_loc};
                   ident_loc        = Location.mkloc label def_loc
                 }
               end c_lhs.pat_extra)
           in
           List.iter f defs;
           Opt.may c_guard fe;
           fe c_rhs;
        end cases;
    | Texp_match (expr, cl, _) ->
      fe expr;
      List.iter begin fun { c_lhs = p; c_guard = oe; c_rhs = e } ->
        List.iter (fun d -> d.ident_kind <- Def {def_name=""; def_loc=none; def_scope=e.exp_loc}; f d) (fp p);
        Opt.may oe fe;
        fe e;
      end cl
    | Texp_apply (expr, pe) ->
      fe expr;
      List.iter (fun (_, e) -> Opt.may e fe) pe;
    | Texp_try (expr, pe) ->
      fe expr;
      List.iter begin fun { c_lhs = p; c_guard = oe; c_rhs = e }  ->
        List.iter (fun d -> d.ident_kind <- Def {def_name=""; def_loc=none; def_scope=e.exp_loc}; f d) (fp p);
        Opt.may oe fe;
        fe e;
      end pe
    | Texp_tuple el -> List.iter fe el
    | Texp_construct ({ Asttypes.loc; _ }, cd, el) ->
      let type_expr = lid_of_type_expr cd.Types.cstr_res in
      let path, is_qualified =
        match cd.Types.cstr_tag with
          | Types.Cstr_extension (p, _) ->
            let path = Longident.parse (Path.name p) in
            path, Longident.qualified path
          | Types.Cstr_constant _
          | Types.Cstr_block _
          | Types.Cstr_unboxed ->
            Longident.of_type_expr type_expr cd.Types.cstr_name,
            Longident.qualified type_expr
      in
      let ident_kind = if is_qualified then Ext_ref else Int_ref none in
      let ident = {
        ident_kind;
        ident_fname = "";
        ident_loc   = Location.mkloc (Odoc_misc.string_of_longident path) loc;
      } in
      f ident;
      List.iter fe el
    | Texp_variant (_, expr) ->
      Opt.may expr fe
    | Texp_record { fields; extended_expression; _ } ->
      Array.iter begin fun (label_description, record_label_definition) ->
        let { Types.lbl_name; lbl_res; lbl_loc; _ } = label_description in
        let type_expr = lid_of_type_expr lbl_res in
        let ident_kind = if Longident.qualified type_expr then Ext_ref else Int_ref none in
        let path = Longident.of_type_expr type_expr lbl_name in
        let ident = {
          ident_kind;
          ident_fname = "";
          ident_loc   = Location.mkloc (Odoc_misc.string_of_longident path) lbl_loc;
        } in
        f ident;
        match record_label_definition with
        | Kept _ -> ()
        | Overridden (_, expr) -> fe expr
      end fields;
      Opt.may extended_expression fe
    | Texp_field (expr, { Asttypes.loc; _ }, ld) ->
      let type_expr = lid_of_type_expr ld.Types.lbl_res in
      let ident_kind = if Longident.qualified type_expr then Ext_ref else Int_ref none in
      let path = Longident.of_type_expr type_expr ld.Types.lbl_name in
      let ident = {
        ident_kind;
        ident_fname = "";
        ident_loc   = Location.mkloc (Odoc_misc.string_of_longident path) loc;
      } in
      f ident;
      fe expr
    | Texp_setfield (e1, { Asttypes.loc; _ }, ld, e2) ->
      let type_expr = lid_of_type_expr ld.Types.lbl_res in
      let ident_kind = if Longident.qualified type_expr then Ext_ref else Int_ref none in
      let path = Longident.of_type_expr type_expr ld.Types.lbl_name in
      let ident = {
        ident_kind;
        ident_fname = "";
        ident_loc   = Location.mkloc (Odoc_misc.string_of_longident path) loc;
      } in
      f ident;
      fe e1;
      fe e2
    | Texp_array el -> List.iter fe el
    | Texp_ifthenelse (e1, e2, e3) -> fe e1; fe e2; Opt.may e3 fe
    | Texp_sequence (e1, e2) -> fe e1; fe e2
    | Texp_while (e1, e2) -> fe e1; fe e2
    | Texp_for (_, _, e1, e2, _, e3) -> fe e1; fe e2; fe e3
    | Texp_send (e1, _meth, e2) ->
     (* let name = match meth with Tmeth_name x -> "(M) " ^ x | Tmeth_val id -> "(V) " ^ (Ident.name id) in
      Log.println `TRACE "Texp_send: %s %s [%s]"
        name (string_of_loc e1.exp_loc) (match e2 with Some e -> string_of_loc e.exp_loc | _ -> "");*)
      fe e1;
      Opt.may e2 fe
    | Texp_new (_, _, _class_decl) -> ()
    | Texp_instvar (_p, _path, _loc) -> ()
      (*Log.println `TRACE "Texp_instvar: (%s)%s %s" (Path.name p) (Path.name path) (string_of_loc loc.loc);*)
    | Texp_setinstvar (_, _, _, expr) -> fe expr
    | Texp_override (_, ll) -> List.iter (fun (_, _, e) -> fe e) ll
    | Texp_letmodule (_, mod_name, _, mod_expr, expr) ->
      begin match mod_name with
        | { Asttypes.txt = Some name; loc } -> 
        f {
                ident_kind  = Def_module { def_name = name; def_loc = loc; def_scope = mod_expr.mod_loc };
        ident_fname = "";
        ident_loc   = { Asttypes.txt = name; loc };
      }
      | { Asttypes.txt = None; _ } -> ()
      end;
      iter_module_expr f mod_expr;
      fe expr;
    | Texp_assert expr -> fe expr
    | Texp_lazy expr -> fe expr
    | Texp_object (cl_str, _) -> iter_class_structure f cl_str
    | Texp_pack mod_expr -> iter_module_expr f mod_expr
    | Texp_constant _ -> ()
    (* Since 4.03 *)
    | Texp_unreachable -> ()
    (* type t = ..;; type t += I of int;; [%extension_constructor I] *)
    | Texp_extension_constructor ({ Asttypes.txt; loc }, _) ->
      let ident_kind = if Longident.qualified txt then Ext_ref else Int_ref none in
      f { ident_kind
        ; ident_fname = ""
        ; ident_loc = Location.mkloc (Odoc_misc.string_of_longident txt) loc
        }
    (* Since 4.08 TODO: *)
    | Texp_letop _ -> ()
    | Texp_open _ -> ()

(** iter_module_expr *)
and iter_module_expr f {mod_desc; mod_loc; _} =
  match mod_desc with
    | Tmod_structure {str_items; _} ->
      let annots = List.fold_left (fun acc item -> (iter_structure_item f item) @ acc) [] str_items in
      List.iter begin function [@warning "-4"]
        | {ident_kind = Open loc; _} as annot -> annot.ident_kind <- Open {loc with loc_end = mod_loc.loc_end}
        | _ -> ()
      end annots;
    | Tmod_functor (_, mod_expr) -> iter_module_expr f mod_expr
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
    | Tmty_alias _ -> ()
    | Tmty_signature sign ->
      let annots = List.fold_left (fun acc item -> (iter_signature_item f item) @ acc) [] sign.sig_items in
      List.iter begin function [@warning "-4"]
        | {ident_kind = Open loc; _} as annot -> annot.ident_kind <- Open {loc with loc_end = mty_loc.loc_end}
        | _ -> ()
      end annots;
    | Tmty_functor (Unit, mt) ->
      iter_module_type f mt
    | Tmty_functor (Named (_, _, mt1), mt2) ->
                    iter_module_type f mt1;
                    iter_module_type f mt2; 
    | Tmty_with (mt, ll) ->
      iter_module_type f mt;
      List.iter (fun (_, _, wc) -> iter_with_constraint f wc) ll
    | Tmty_typeof me -> iter_module_expr f me

(** iter_signature_item *)
and iter_signature_item f {sig_desc; _} =
  let fmt = iter_module_type f in
  match sig_desc with
    | Tsig_value vdesc ->
      iter_value_description f vdesc;
      []
    | Tsig_type (_, ll) ->
      List.iter (fun td -> iter_type_declaration f td) ll;
      []
    | Tsig_exception { tyexn_constructor; _ } ->
      iter_extension_constructor f tyexn_constructor;
      []
    | Tsig_module mdecl ->
      iter_module_type f mdecl.md_type;
      []
    | Tsig_recmodule ll ->
      List.iter (fun mdecl -> fmt mdecl.md_type) ll;
      []
    | Tsig_modtype mtdecl ->
      Opt.may mtdecl.mtd_type fmt;
      []
    (*| Tsig_open { open_path; open_txt; _ } ->
      let { Asttypes.loc; _ } = open_txt in
      let annot = {
        ident_kind  = Open {loc_start = loc.loc_end; loc_end = dummy_pos; loc_ghost=false};
        ident_fname = "";
        ident_loc   = Location.mkloc (Path.name open_path) loc;
      } in
      f annot;
      [annot]*)
    (* Since 4.08 TODO: Check correctness *)
    | Tsig_open { open_expr; _ } ->
      let (open_path, open_txt) = open_expr in
      let { Asttypes.loc; _ } = open_txt in
      let annot = {
        ident_kind  = Open {loc_start = loc.loc_end; loc_end = dummy_pos; loc_ghost=false};
        ident_fname = "";
        ident_loc   = Location.mkloc (Path.name open_path) loc;
      } in
      f annot;
      [annot]

    | Tsig_include idecl ->
      fmt idecl.incl_mod;
      []
    | Tsig_class _ -> []
    | Tsig_class_type _ -> []
    (* Added in 4.02.0 -- TODO *)
    | Tsig_typext { tyext_constructors; _ } ->
      List.iter (iter_extension_constructor f) tyext_constructors;
      []
    | Tsig_attribute _ -> []
    (* From 4.08 *)
    | Tsig_typesubst ll ->
      List.iter (fun td -> iter_type_declaration f td) ll;
      []
    (* From 4.08 TODO: *)
    | Tsig_modsubst mod_subst ->
      iter_module_substitution mod_subst;
      []

(** iter_value_description *)
and iter_value_description f {val_desc; _} = iter_core_type f val_desc

(** iter_with_constraint *)
and iter_with_constraint f = function
  | Twith_type td -> iter_type_declaration f td
  | Twith_module _ -> ()
  | Twith_typesubst td -> iter_type_declaration f td
  | Twith_modsubst _ -> ()

(** iter_core_type *)
and iter_core_type _f ?loc:_ {ctyp_desc = _; _} = ()

(** iter_class_field *)
and iter_class_field f {cf_desc; _} =
  match cf_desc with
    | Tcf_val (_, _, _, Tcfk_virtual core_type, _) -> iter_core_type f core_type
    | Tcf_val (_, _, _, Tcfk_concrete (_, expr), _) ->
      (*Log.println `TRACE "Tcf_val: %s %s" name (string_of_loc loc.loc);*)
      ignore (iter_expression f expr)
    | Tcf_method (_, _, Tcfk_virtual core_type) -> iter_core_type f (*~loc:loc.loc*) core_type
    | Tcf_method (_, _, Tcfk_concrete (_, expr)) ->
      (*Log.println `TRACE "Tcf_meth: %s %s" name (string_of_loc loc.loc);*)
      ignore (iter_expression f (*~loc:loc.loc*) expr)
    | Tcf_constraint (ct1, ct2) ->
      iter_core_type f ct1;
      iter_core_type f ct2;
    | Tcf_initializer expr -> ignore (iter_expression f expr)
    | Tcf_inherit (_, class_expr, _, _, _) -> iter_class_expr f class_expr
    (* Added in 4.02.0 *)
    | Tcf_attribute _ -> ()

(** iter_class_structure *)
and iter_class_structure f {cstr_fields; _} = List.iter (iter_class_field f) cstr_fields

(** iter_class_expr *)
and iter_class_expr f {cl_desc; _} =
  match cl_desc with
    | Tcl_ident (_, _, core_type) -> List.iter (iter_core_type ?loc:None f) core_type
    | Tcl_structure str -> iter_class_structure f str
    | Tcl_fun (_, pat, _ll, expr, _) ->
      let defs = iter_pattern f pat in
      List.iter (fun d -> d.ident_kind <- Def {def_name=""; def_loc=none; def_scope=expr.cl_loc}; f d) defs;
      (* TODO: Why ?  Same question for Tcl_let *)
      (*List.iter (fun (_, expr) -> (ignore (iter_expression f expr)) _ll;*)
      iter_class_expr f expr
    | Tcl_apply (expr, ll) ->
      iter_class_expr f expr;
      List.iter (function (_, Some expr) -> ignore (iter_expression f expr) | _ -> ()) ll;
    | Tcl_let (_, ll, _ll2, c_expr) ->
      List.iter begin fun { vb_pat = p; vb_expr = e; _ } ->
        let defs = iter_pattern f p in
        List.iter (fun d -> d.ident_kind <- Def {def_name=""; def_loc=none; def_scope=c_expr.cl_loc}; f d) defs;
        (*List.iter (fun (_, expr) -> ignore (iter_expression f expr)) _ll2;*)
        iter_expression f e;
      end ll;
      iter_class_expr f c_expr;
    | Tcl_constraint (cle, clt, _, _, _) ->
      iter_class_expr f cle;
      Opt.may clt (iter_class_type f)
    (* added in 4.06 *)
    | Tcl_open (_, c_expr) ->
      iter_class_expr f c_expr

(** iter_class_type *)
and iter_class_type f {cltyp_desc; _} =
  match cltyp_desc with
    | Tcty_constr (_, _, ct) -> List.iter (iter_core_type ?loc:None f) ct
    | Tcty_signature sign -> iter_class_signature f sign
    | Tcty_arrow (_, ct, clt) ->
      iter_core_type f ct;
      iter_class_type f clt
    (* Added in 4.06 *)
    | Tcty_open (_, class_type) ->
      iter_class_type f class_type

(** iter_class_type_field *)
and iter_class_type_field f {ctf_desc; _} =
  match ctf_desc with
    | Tctf_inherit clt -> iter_class_type f clt
    | Tctf_val (_, _, _, ct) -> iter_core_type f ct
    | Tctf_method (_, _, _, ct) -> iter_core_type f ct
    | Tctf_constraint (ct1, ct2) ->
      iter_core_type f ct1;
      iter_core_type f ct2;
    (* Added in 4.02.0 - TODO *)
    | Tctf_attribute _ -> ()

(** iter_class_signature *)
and iter_class_signature f {csig_fields; _} = List.iter (iter_class_type_field f) csig_fields

(** iter_type_declaration *)
and iter_type_declaration f {typ_kind; typ_manifest; typ_cstrs; _} =
  begin
    match typ_kind with
      | Ttype_abstract -> ()
      | Ttype_variant ll ->
        List.iter begin fun { cd_name; cd_args; _ } ->
          let { Asttypes.txt; loc } = cd_name in
          let ident_kind = Def_constr { def_name = txt; def_loc = loc; def_scope = none } in
          f {
            ident_kind;
            ident_fname = "";
            ident_loc   = cd_name;
          };
          match cd_args with
          | Cstr_tuple ct  -> List.iter (iter_core_type ?loc:None f) ct
          | Cstr_record ll -> List.iter begin fun { ld_name; ld_type = ct; _ } ->
              let { Asttypes.loc; txt } = ld_name in
              let ident_kind = Def_constr { def_name = txt; def_loc = loc; def_scope = none } in
              f {
                ident_kind;
                ident_fname = "";
                ident_loc   = ld_name;
              };
              iter_core_type f ct
            end ll
       end ll
      | Ttype_record ll ->
        List.iter begin fun { ld_name; ld_type; _ } ->
          let { Asttypes.txt; loc } = ld_name in
          let ident_kind = Def_constr { def_name = txt; def_loc = loc; def_scope = none } in
          f {
            ident_kind;
            ident_fname = "";
            ident_loc   = ld_name;
          };
          iter_core_type f ld_type
        end ll
      (* Added in 4.02.0 *)
      | Ttype_open -> ()
  end;
  List.iter begin fun (ct1, ct2, _) ->
    iter_core_type f ct1;
    iter_core_type f ct2;
  end typ_cstrs;
  Opt.may typ_manifest (iter_core_type ?loc:None f);

(** iter_extension_constructor - Added in 4.02 *)
and iter_extension_constructor f { ext_name; ext_kind; _ }  =
  match ext_kind with
  | Text_decl (arguments, core_type_opt) ->
    let { Asttypes.txt; loc } = ext_name in
    let ident_kind = Def_constr { def_name = txt; def_loc = loc; def_scope = none } in
    f { ident_kind;
        ident_fname = "";
        ident_loc = ext_name };
    begin match arguments with
      | Cstr_tuple core_types ->
        List.iter (iter_core_type ?loc:None f) core_types
      | Cstr_record label_declarations ->
        List.iter (fun ld -> iter_core_type f ld.ld_type) label_declarations
      end;
    Opt.may core_type_opt (iter_core_type ?loc:None f)
      (*TODO*)
  | Text_rebind (_path, _id ) -> ()

(** iter_module_substitution - Added in 4.08 TODO: ? *)
and iter_module_substitution { ms_name; ms_loc; _ } = ()

(** iter_structure_item *)
and iter_structure_item f {str_desc; str_loc; _} =
  match str_desc with
    | Tstr_eval (expr, _) ->
      ignore (iter_expression f expr);
      []
    | Tstr_value (_, pe) ->
      let defs =
        List.rev (List.fold_left begin fun acc { vb_pat; vb_expr; _ } ->
          let defs = iter_pattern f vb_pat in
          ignore (iter_expression f vb_expr);
          defs @ acc
        end [] pe)
      in
      List.iter (fun d -> d.ident_kind <-
        Def {def_name=""; def_loc=none; def_scope={str_loc with loc_start = str_loc.loc_end; loc_end = Lexing.dummy_pos}}) defs;
      List.iter f defs;
      []
    | Tstr_open open_declaration ->
      (*let { open_path = path; open_loc = loc; _ } = odesc in
      let annot = {
        ident_kind  = Open {loc_start = loc.loc_end; loc_end = dummy_pos; loc_ghost=false};
        ident_fname = "";
        ident_loc   = Location.mkloc (Path.name path) loc;
      } in
      f annot;
      [annot]*)
    (* Changed in 4.08 TODO *)
      []
    | Tstr_include _ -> []
    | Tstr_class ll -> List.iter (fun (cd, _) ->
      iter_class_expr f cd.ci_expr) ll;
      []
    | Tstr_class_type ll -> List.iter (fun (_, _, cd) ->
      iter_class_type f cd.ci_expr) ll;
      []
    | Tstr_type (_, ll) -> List.iter (fun td ->
      iter_type_declaration f td) ll;
      []
    | Tstr_exception { tyexn_constructor; _ } ->
      iter_extension_constructor f tyexn_constructor;
      []
    | Tstr_module { mb_name; mb_expr; _ } ->
      let { Asttypes.txt; loc } = mb_name in
      begin match txt with 
      | Some name ->
      f {
        ident_kind  = Def_module { def_name = name; def_loc = loc; def_scope = mb_expr.mod_loc };
        ident_fname = "";
        ident_loc   = { Asttypes.txt = name; loc };
      }
      | None -> ()
      end;
      iter_module_expr f mb_expr;
      []
    | Tstr_modtype { mtd_name; mtd_type; _ } ->
      let { Asttypes.txt; loc } = mtd_name in
      begin match mtd_type with
        | Some mt ->
          f {
            ident_kind  = Def_module { def_name = txt; def_loc = loc; def_scope = loc; };
            ident_fname = "";
            ident_loc   = mtd_name;
          };
          iter_module_type f mt;
        | None -> ()
      end;
      []
    | Tstr_recmodule ll ->
      List.iter begin fun { mb_name; mb_expr; _ } ->
        let { Asttypes.txt; loc } = mb_name in
        begin match txt with
        | Some name ->
        f {
          ident_kind  = Def_module { def_name = name; def_loc = loc; def_scope = mb_expr.mod_loc };
          ident_fname = "";
          ident_loc   = { Asttypes.txt = name; loc };
        }
        | None -> ()
        end;
        (*iter_module_type f mt;*)
        iter_module_expr f mb_expr;
      end ll;
      []
    | Tstr_primitive vdecl ->
      iter_value_description f vdecl;
      []
    (* Added in 4.02.0 *)
    | Tstr_typext { tyext_constructors; _ } ->
      List.iter (iter_extension_constructor f) tyext_constructors;
      []
    | Tstr_attribute _ -> []

;;

(** register *)
let register filename entry ({ident_loc; ident_kind; _} as ident) =
  let { Asttypes.loc; _ } = ident_loc in
  if loc <> Location.none then begin
    ident.ident_fname <- filename;
    let start = loc.loc_start.pos_cnum in
    let stop  = loc.loc_end.pos_cnum in
    for i = start to stop do entry.locations.(i) <- Some ident done;
    match ident_kind with
      | Int_ref x when x = none ->
        begin
          match List_opt.find begin fun {def_name; def_scope; _} ->
            def_name = ident.ident_loc.txt  && (def_scope <== start || def_scope = none)
          end entry.definitions with
            | Some def ->
              ident.ident_kind <- (Int_ref def.def_loc);
              Hashtbl.add entry.int_refs def.def_loc ident;
            | _ -> () (*assert false*)
        end;
      | Int_ref def_loc (*when List.exists (fun d -> d.def_loc = def_loc) entry.definitions*) ->
        (* When Int_ref refers to a recursive function, its def is not yet regstered in entry.definitions *)
        Hashtbl.add entry.int_refs def_loc ident;
      (*| Int_ref _ -> (* This case fixes the wrong location of the Int_ref definition when Int_ref refers to an alias. *)
        let def_loc =
          List_opt.find begin fun d ->
            d.def_name = ident.ident_loc.txt && d.def_scope <== ident.ident_loc.loc.loc_start.pos_cnum
          end entry.definitions
        in
        Opt.may def_loc begin fun {def_loc; _} ->
          let ident = {ident with ident_kind = Int_ref def_loc} in
          Hashtbl.add entry.int_refs def_loc ident;
          for i = ident.ident_loc.loc.loc_start.pos_cnum to ident.ident_loc.loc.loc_end.pos_cnum do entry.locations.(i) <- Some ident done;
        end*)
      | Ext_ref | Open _ ->
        begin
          match Longident.flatten (Longident.parse ident.ident_loc.txt) with
            | modname :: _ -> Hashtbl.add entry.ext_refs modname ident
            | _ -> ()
        end;
      | (Def def | Def_constr def | Def_module def) as ident_def ->
        if def.def_name = "" then (def.def_name <- ident_loc.txt);
        if def.def_loc = Location.none then def.def_loc <- loc;
        begin
          match ident_def with
            | Def_module def ->
              let parents = List.filter (fun d -> d.def_scope <== def.def_loc.loc_start.pos_cnum) entry.definitions in
              let parents = List.map (fun d -> d.def_name) parents in
              let path = if parents = [] then (Miscellanea.modname_of_path filename) else String.concat "." parents in
              def.def_name <- String.concat "." (List.filter ((<>) "") [path; def.def_name]);
            | Def _ | Def_constr _ | Int_ref _ | Ext_ref | Open _ -> ()
        end;
        entry.definitions <- def :: entry.definitions;
  end

(** scan *)
let critical_scan = Mutex.create()

let scan ~project ~filename ?compile_buffer () =
  Mutex.lock critical_scan;
  try
    begin
      let timestamp = try (Hashtbl.find table_idents filename).timestamp with Not_found -> 0. in
      match Binannot.read_cmt ~project ~filename ~timestamp ?compile_buffer () with
        | Some (filename, timestamp, ({cmt_sourcefile = Some cmt_sourcefile; _} as cmt)) ->
          let size = (Unix.stat (cmt.cmt_builddir // cmt_sourcefile)).Unix.st_size + 1 in
          let entry = {
            timestamp;
            locations   = Array.make size None;
            int_refs    = Hashtbl.create 7;
            ext_refs    = Hashtbl.create 7;
            definitions = [];
          } in
          Hashtbl.replace table_idents filename entry;
          let f = register filename entry in
          begin
            match [@warning "-4"] cmt.cmt_annots with
              | Implementation {str_items; _} ->
                List.iter (fun item -> ignore (iter_structure_item f item)) str_items;
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

(** scan_project_files *)
let scan_project_files ~project ?(sort=true) f =
  let src_files = File_util.readdirs ~links:false (Some (fun x -> x ^^^ ".ml")) (project.Prj.root // Prj.default_dir_src) in
  let src_files = if sort then List.sort compare src_files else src_files in
  List.fold_left begin fun acc filename ->
    scan ~project ~filename ();
    try
      let entry = Hashtbl.find table_idents filename in
      f acc filename entry
    with Not_found -> acc
  end [] src_files
;;

