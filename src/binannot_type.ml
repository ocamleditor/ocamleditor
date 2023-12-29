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


open Cmt_format
open Typedtree
open Tast_iterator

(** [Lexing.postion] copy *)
type position = Lexing.position = {
  pos_fname : string;
  pos_lnum  : int;
  pos_bol   : int;
  pos_cnum  : int;
}

(** [Location.t] copy *)
type location = Location.t = {
  loc_start : position;
  loc_end   : position;
  loc_ghost : bool;
}

(** ['a Location.loc] & ['a Asttypes.loc] copy *)
type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : location;
}

type t = {
  ba_loc  : location;
  ba_type : string;
}

exception Found of t

module Log = Common.Log.Make(struct let prefix = "Binannot_type" end)
let _ = Log.set_verbosity `TRACE

(** [expr_loc] - to extend the expression [Location.t] to include the coercion.
    Does nothing if there is not coercion. Example of coercion:

    [(expr :> type)]*)
let expr_loc expr =
  let { exp_loc; exp_extra; _ } = expr in
  match exp_extra with
  | (Texp_constraint _, _, _) :: (Texp_coerce (_, target), _, _) :: _
  | (Texp_coerce (_, target), _, _) :: _ -> { exp_loc with loc_end = target.ctyp_loc.loc_end }
  | _ -> exp_loc

(** [lookup offset] will return a [Tast_iterator.iterator] which will traverse
    a [Typedtree] type in depth so that the offset remains within the type
    [loc_start], [loc_end] locations.

    When the most precise type is found an exception may be thrown which will mean
    we found something relevant to display.
*)
let lookup offset =
  let super = Tast_iterator.default_iterator in
  let pat iterator (type k) (pattern : k Typedtree.general_pattern) =
    let { pat_desc; pat_loc; pat_type; _} = pattern in
    let lo = pat_loc.loc_start.pos_cnum in
    let hi = pat_loc.loc_end.pos_cnum in
    if lo <= offset && offset <= hi then begin
      super.pat iterator pattern;

      match pat_desc with
      | Tpat_any
      | Tpat_constant _
      | Tpat_construct _
      | Tpat_var _
      | Tpat_alias _  -> raise @@ Found { ba_loc = pat_loc; ba_type = Odoc_info.string_of_type_expr pat_type }
      | _ -> ()
    end
  in

  let expr iterator (expression : Typedtree.expression) =
    let { exp_desc; exp_type; _ } = expression in
    let exp_loc = expr_loc expression in
    let lo = exp_loc.loc_start.pos_cnum in
    let hi = exp_loc.loc_end.pos_cnum in
    if lo <= offset && offset <= hi then begin
      super.expr iterator expression;

      begin match exp_desc with
      | Texp_constant _
      | Texp_construct _
      | Texp_variant _
      | Texp_record _
      | Texp_array _
      | Texp_field _
      | Texp_object _
      | Texp_new _
      | Texp_send _
      | Texp_override _
      | Texp_instvar _ ->
          raise @@ Found { ba_loc = exp_loc; ba_type = Odoc_info.string_of_type_expr exp_type }
      (* The [exp_type] for this variants is just [unit] which is not very interesting.
         Also [exp_loc] covers the whole expression not just the field/val name *)
      | Texp_setfield (_, { loc; _ }, _, e)
      | Texp_setinstvar (_, _, { loc; _ }, e) ->
          raise @@ Found { ba_loc = loc; ba_type = Odoc_info.string_of_type_expr e.exp_type }
      | Texp_ident (_path, _, value_desc) ->
          begin
            let open Types in
            match value_desc.val_kind with
            | Val_self (cls_sig, _meths, _vars, _) ->
                (*Printf.printf " %s\n%!" (Odoc_info.string_of_type_expr cls_sig.csig_self);
                  cls_sig.csig_meths |> Meths.iter (fun m (priv, virt, te) ->
                    Printf.printf "  %s %s %s %s\n%!" m
                      (Odoc_info.string_of_type_expr te)
                      (match priv with Mpublic -> "" | Mprivate _ -> "private")
                      (match virt with Asttypes.Virtual -> "V" | Asttypes.Concrete -> "C"));*)
                let ba_type =
                  Meths.fold (fun name (_priv, _virt, te) acc -> (name, Odoc_info.string_of_type_expr te) :: acc) cls_sig.csig_meths []
                  |> List.map (fun (n, t) -> Printf.sprintf "%s : %s" n t) |> String.concat "; "
                  |> Printf.sprintf "< %s; .. >"
                in
                raise @@ Found { ba_loc = exp_loc; ba_type };
            | Val_reg | Val_prim _ | Val_ivar _ | Val_anc _ ->
                raise @@ Found { ba_loc = exp_loc; ba_type = Odoc_info.string_of_type_expr exp_type }
          end;
      | _ -> ()
      end
    end
  in

  let class_field iterator class_field =
    let { cf_desc; cf_loc; _ } = class_field in
    let lo = cf_loc.loc_start.pos_cnum in
    let hi = cf_loc.loc_end.pos_cnum in
    if lo <= offset && offset <= hi then begin
      super.class_field iterator class_field;

      match cf_desc with
      (* fields, btw virtual fields ? *)
      | Tcf_val (name, _, _, Tcfk_virtual core_type, _) ->
          raise @@ Found { ba_loc = name.loc; ba_type = Odoc_info.string_of_type_expr core_type.ctyp_type }
      | Tcf_val (name, _, _, Tcfk_concrete (_, expr), _) ->
          raise @@ Found { ba_loc = name.loc; ba_type = Odoc_info.string_of_type_expr expr.exp_type }

      (* methods *)
      | Tcf_method (name, _, Tcfk_virtual core_type) ->
          raise @@ Found { ba_loc = name.loc; ba_type = Odoc_info.string_of_type_expr core_type.ctyp_type }
      | Tcf_method (name, _, Tcfk_concrete (_, { exp_desc = Texp_function { cases ; _ }; _ })) ->
          (* List.hd cases is hack, of course *)
          raise @@ Found { ba_loc = name.loc; ba_type = Odoc_info.string_of_type_expr (List.hd cases).c_rhs.exp_type }

      | _ -> ()
    end
  in

  let typ iterator core_type =
    let { Typedtree.ctyp_type; ctyp_loc; _ } = core_type in
    let lo = ctyp_loc.loc_start.pos_cnum in
    let hi = ctyp_loc.loc_end.pos_cnum in
    if lo <= offset && offset <= hi then begin
      super.typ iterator core_type;

      raise @@ Found { ba_loc = ctyp_loc; ba_type = Odoc_info.string_of_type_expr ctyp_type }
    end
  in

  { super with pat; expr; class_field; typ }

let find_part_iterator iterator = function
  | Partial_structure {str_items; _} -> List.iter (iterator.structure_item iterator) str_items
  | Partial_structure_item item -> iterator.structure_item iterator item
  | Partial_expression expr -> ignore (iterator.expr iterator expr)
  | Partial_pattern (_, pat) -> ignore (iterator.pat iterator pat)
  | Partial_class_expr cl_expr -> ignore (iterator.class_expr iterator cl_expr)
  | Partial_signature sigs ->List.iter (iterator.signature_item iterator) sigs.sig_items
  | Partial_signature_item sig_item -> iterator.signature_item iterator sig_item
  | Partial_module_type mtyp -> iterator.module_type iterator mtyp;;

(** [find_by_offset ~project ~filename ~offset ?compile_buffer ()]

    Read the [.cmt] file of a given filename in in project, compiling it if
    neeeded and find the type at the given offset.
*)
let find_by_offset ~project ~filename ~offset ?compile_buffer () =
  match Binannot.read_cmt ~project ~filename ?compile_buffer () with
  | Some (_, _, cmt) ->
      begin
        try
          Odoc_info.reset_type_names();
          begin
            let iterator = lookup offset in
            match cmt.cmt_annots with
            | Implementation {str_items; _} -> List.iter (iterator.structure_item iterator) str_items
            | Interface { sig_items; _ } -> List.iter (iterator.signature_item iterator) sig_items
            | Partial_implementation parts
            | Partial_interface parts  -> Array.iter (find_part_iterator iterator) parts
            | Packed _ -> ()
          end;
          None
        with Found ba -> Some ba
      end;
  | _ -> None


(** [find ~page ?iter ()]

    Find the OCaml type in page either at the given [iter] or at the insert position
*)
let find ~page ?iter () =
  let compile_buffer () = page#compile_buffer ?join:(Some true) () in
  let iter = match iter with Some it -> it | _ -> page#buffer#get_iter `INSERT in
  find_by_offset ~project:page#project ~filename:page#get_filename ~offset:iter#offset ~compile_buffer ()
