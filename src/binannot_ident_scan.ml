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

open Utils
open Cmt_format
open Typedtree

module Log = Common.Log.Make(struct let prefix = "Binannot_ident_scan" end)
let _ = Log.set_verbosity `TRACE

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

(** ['a Location.loc] copy *)
type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : location;
}

(** [Binannot.definiton] copy *)
type definition = Binannot.definition = {
  mutable def_name  : string;
  mutable def_loc   : location;
  mutable def_scope : location;
}

(** [Binannot.ident_kind] copy *)
type ident_kind = Binannot.ident_kind =
  | Def of definition
  | Def_constr of definition
  | Def_module of definition
  | Int_ref of location
  | Ext_ref
  | Open of location

(** [Binannot.ident] copy *)
type ident = Binannot.ident = {
  mutable ident_fname : string;
  mutable ident_kind  : ident_kind;
  ident_loc           : string loc;
}

(** [Binannot.entry] copy *)
type entry = Binannot.entry = {
  timestamp           : float;
  locations           : ident option array;
  int_refs            : (location, ident) Hashtbl.t;
  ext_refs            : (string, ident) Hashtbl.t;
  mutable definitions : definition list;
}

exception Found of ident

type ext_def = Project_def of ident | Project_file of ident | Library_def | No_def

(** [same_file] - true if both location refer to same file *)
let same_file loc loc' =
  loc.loc_start.pos_fname = loc'.loc_start.pos_fname

(** [pat_iterator] *)
let pat_iterator ~f ~filename ~scope =
  let open Tast_iterator in
  let super = default_iterator in
  let ident_fname = filename in

  let pat iterator (type k) (pattern : k Typedtree.general_pattern) =
    begin match [@warning "-4"] pattern.pat_desc with
    | Tpat_var (id, loc)
    | Tpat_alias (_, id, loc) ->
        let definition = { def_name = Ident.name id; def_loc = loc.loc; def_scope = scope } in
        f { ident_fname; ident_kind  = Def definition; ident_loc = loc }

    | Tpat_construct (lid, desc, _, _) ->
        let { Types.cstr_loc; _ } = desc in
        let name = Longident.flatten lid.txt |> String.concat "." in
        let ident_loc = Location.mkloc name lid.loc in
        let ident_kind = if same_file lid.loc cstr_loc
          then Int_ref cstr_loc
          else Ext_ref
        in
        f { ident_fname; ident_kind; ident_loc }

    | Tpat_record (labels, _) ->
        List.iter
          (fun (lid, lbl_desc, _) ->
             let name = Longident.flatten lid.txt |> String.concat "." in
             let ident_loc = Location.mkloc name lid.loc in
             let { Types.lbl_loc; _ } = lbl_desc in
             let ident_kind = if same_file lid.loc lbl_loc
               then Int_ref lbl_loc
               else Ext_ref
             in
             f { ident_fname; ident_kind; ident_loc }
          )
          labels

    | _ -> ()
    end;
    super.pat iterator pattern
  in
  { super with pat }

(** [handle_pattern] *)
let handle_pattern ~f ~filename ~scope =
  let open Tast_iterator in
  let iter = pat_iterator ~f ~filename ~scope in
  fun p -> iter.pat iter p

(** [handle_bindings] *)
let handle_bindings ~f ~filename ~scope bindings =
  let handler = handle_pattern ~f ~filename ~scope in
  List.iter
    (fun vb ->
       let { vb_pat; _ } = vb in
       handler vb_pat)
    bindings

(** [handle_cases] *)
let handle_cases ~f ~filename cases =
  List.iter
    (fun { c_lhs; c_guard; c_rhs } ->
       let scope = match c_guard with
         | None -> c_rhs.exp_loc
         | Some guard ->
             { c_rhs.exp_loc with loc_start = guard.exp_loc.loc_start }
       in
       handle_pattern ~f ~filename ~scope c_lhs
    )
    cases

let rec iterator ~f ~filename ~scope =
  let open Tast_iterator in
  let super = default_iterator in
  let ident_fname = filename in

  let class_expr iter node =
    super.class_expr iter node
  in
  let module_expr _iter node =
    let { mod_loc; _ } = node in
    super.module_expr (iterator ~f ~filename ~scope:mod_loc) node
  in
  let expr iter node =
    let { exp_desc; exp_loc; _ } = node in
    begin match exp_desc with
    | Texp_ident (_, { txt; loc }, desc) ->
        let name = Longident.flatten txt |> String.concat "." in
        let ident_loc = Location.mkloc name loc in
        let { Types.val_loc; _ } = desc in
        let ident_kind = if same_file loc val_loc
          then Int_ref val_loc
          else Ext_ref
        in
        f { ident_fname; ident_kind; ident_loc }

    | Texp_let (Asttypes.Recursive, bindings, _ ) ->
        handle_bindings ~f ~filename ~scope:exp_loc bindings

    | Texp_let (Asttypes.Nonrecursive, bindings, body) ->
        let { Typedtree.exp_loc = body_exp_loc; _ } = body in
        handle_bindings ~f ~filename ~scope:body_exp_loc bindings

    | Texp_match (_, cases, _) ->
        handle_cases ~f ~filename cases

    | Texp_function { arg_label; param; cases; _ } ->
        (* TODO: arg_label, param *)
        ignore arg_label;
        ignore param;
        handle_cases ~f ~filename cases

    | Texp_try (_, cases) ->
        handle_cases ~f ~filename cases

    (* [C] or [C v] or [C(v, u)] where C can be defined in other module *)
    | Texp_construct (lid, desc, _) ->
        let name = Longident.flatten lid.txt |> String.concat "." in
        let { Types.cstr_loc; _ } = desc in
        let ident_loc = Location.mkloc name lid.loc in
        let ident_kind = if same_file lid.loc cstr_loc
          then Int_ref cstr_loc
          else Ext_ref in
        f { ident_fname; ident_kind; ident_loc };

        (* [rec.f <- v] and [rec.f] where [f] can be defined in other module *)
    | Texp_setfield (_, lid, desc, _)
    | Texp_field (_, lid, desc) ->
        let name = Longident.flatten lid.txt |> String.concat "." in
        let { Types.lbl_loc; _ } = desc in
        let ident_loc = Location.mkloc name lid.loc in
        let ident_kind = if same_file lid.loc lbl_loc
          then Int_ref lbl_loc
          else Ext_ref
        in
        f { ident_fname; ident_kind; ident_loc }

    (* In an expression this is [let module M = struct .. end ]

         Not handled yet. Can be nice to have for programming with first class
         modules and such. Not there yet. *)
    | Texp_letmodule _ -> ()

    | _ -> ()
    end;
    super.expr iter node
  in
  let structure_item_rem iter node rem =
    let { Typedtree.str_desc; str_loc; _ } = node in
    begin match str_desc with
    | Tstr_value (rec_flag, bindings) ->
        let doit loc_start =
          handle_bindings ~f ~filename ~scope:{ scope with loc_start} bindings in
        begin match rec_flag, rem with
        | Asttypes.Recursive, _ -> doit str_loc.loc_start
        | Asttypes.Nonrecursive, [] -> doit str_loc.loc_end
        | Asttypes.Nonrecursive, { Typedtree.str_loc = loc2; _ } :: _ ->
            doit loc2.loc_start
        end

    | _ -> ()
    end;
    super.structure_item iter node
  in
  let structure_item iter node =
    structure_item_rem iter node []
  in
  let structure iter node =
    let rec loop = function
      | str :: rem -> structure_item_rem iter str rem; loop rem
      | [] -> ()
    in
    let { Typedtree.str_items; _ } = node in
    loop str_items
  in

  let type_kind iter node =
    let label_declaration f decl =
      let { Typedtree.ld_name; ld_loc; _ } = decl in
      let ident_kind = Def_constr
          { def_name = ld_name.txt
          ; def_loc  = ld_loc
          ; def_scope = Location.none
          } in
      f { ident_fname; ident_kind; ident_loc = ld_name }
    in
    let constructor_declaration f decl =
      let { Typedtree.cd_name; cd_args; cd_loc; _ } = decl in
      let ident_kind = Def_constr
          { def_name = cd_name.txt
          ; def_loc  = cd_loc
          ; def_scope = Location.none
          } in
      f { ident_fname; ident_kind; ident_loc = cd_name };
      match cd_args with
      | Typedtree.Cstr_record ll -> List.iter (label_declaration f) ll
      | Cstr_tuple _ -> ()
    in

    begin match node with
    | Typedtree.Ttype_variant cl -> List.iter (constructor_declaration f) cl
    | Ttype_record ll -> List.iter (label_declaration f) ll
    | Ttype_abstract
    | Ttype_open -> ()
    end;
    super.type_kind iter node
  in

  let type_extension iter node =
    let { tyext_constructors; _ } = node in
    List.iter
      (fun { Typedtree.ext_name = ident_loc; _ } ->
         let ident_kind =
           Def_constr {
             def_name = ident_loc.txt;
             def_loc  = ident_loc.loc;
             def_scope = Location.none;
           } in
         f { ident_fname; ident_kind; ident_loc }
      )
      tyext_constructors;

    super.type_extension iter node
  in

  { super with class_expr; module_expr; expr; structure_item; structure;
               type_kind; type_extension }

(** register *)
let register filename entry ({ ident_loc; ident_kind; _ } as ident) =

  let ( <== ) = Binannot.( <== ) in
  let loc = begin match ident_kind with
    | Def def | Def_constr def | Def_module def -> def.def_loc
    | Int_ref _ | Ext_ref | Open _ -> ident_loc.loc
  end
  in
  if loc <> Location.none then begin
    ident.ident_fname <- filename;
    let start = loc.loc_start.pos_cnum in
    let stop  = loc.loc_end.pos_cnum in
    for i = start to stop do entry.locations.(i) <- Some ident done;
    begin match ident_kind with
    | Int_ref x when x = Location.none ->
        begin
          match List_opt.find begin fun {def_name; def_scope; _} ->
              def_name = ident.ident_loc.txt  && (def_scope <== start || def_scope = Location.none)
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
          match String.split_on_char '.' ident.ident_loc.txt with
          | modname :: _ -> Hashtbl.add entry.ext_refs modname ident
          | _ -> ()
        end;
    | (Def def | Def_constr def | Def_module def) as ident_def ->
        if def.def_name = "" then (def.def_name <- ident_loc.txt);
        if def.def_loc = Location.none then def.def_loc <- loc;
        entry.definitions <- def :: entry.definitions;
    end;
  end

(** scan *)
let critical_scan = Mutex.create()

let scan ~project ~filename ?compile_buffer () =
  Mutex.lock critical_scan;
  try
    begin
      let timestamp = try (Hashtbl.find Binannot.table_idents filename).timestamp with Not_found -> 0. in
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
          Hashtbl.replace Binannot.table_idents filename entry;
          let f = register filename entry in
          let iter = iterator ~filename ~f ~scope:(Location.in_file filename) in
          begin
            match [@warning "-4"] cmt.cmt_annots with
            | Implementation structure ->
                Tast_iterator.(iter.structure iter structure)

            | _ -> ()
          end
      | _ -> ()
    end;
    Mutex.unlock critical_scan;
  with ex -> begin
      Printf.eprintf "File \"binannot_ident_scan.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
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
      let entry = Hashtbl.find Binannot.table_idents filename in
      f acc filename entry
    with Not_found -> acc
  end [] src_files
;;
