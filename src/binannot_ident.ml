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


open Miscellanea
open Location
open Lexing
open Binannot
open Binannot_ident_scan

type search_result = {
  bai_def  : ident option;
  bai_refs : ident list;
}

(** find_external_definition *)
let find_external_definition ~project ~ext_ref =
  let longid = Longident.parse ext_ref.ident_loc.txt in
  match Longident.flatten longid with
    | name :: _ :: _ | name :: [] ->
      let ext_name = Longident.last longid in
      let longid = Odoc_misc.string_of_longident longid in
      let name = name ^ ".ml" in
      let paths = Project.get_search_path_local project in
      begin
        try
          let filename = Misc.find_in_path_uncap paths name in
          scan ~project ~filename ();
          let {locations; _} = Hashtbl.find table_idents filename in
          begin
            try
              for i = Array.length locations - 1 downto 0 do
                match locations.(i) with
                  | None -> ()
                  | Some ({ident_kind; ident_loc; _} as def_ident) ->
                    match ident_kind with
                      | Def def
                          when def.def_scope.loc_end.pos_cnum = -1 && ident_loc.txt = ext_name ->
                            raise (Found def_ident)
                      | Def_constr def
                          when def.def_name = ext_name -> raise (Found def_ident)
                      | Def_module def
                          when def.def_name = longid -> raise (Found def_ident)
                      | Int_ref _ | Ext_ref | Open _ | Def _ | Def_constr _ | Def_module _ -> ()
              done;
              (* If ident is not found in "locations", return an ident corresponding to the file *)
              Project_file {
                ident_fname  = filename;
                ident_kind   = Def {def_name=ext_ref.ident_loc.txt; def_loc=none; def_scope=none};
                ident_loc    = Location.mkloc ext_ref.ident_loc.txt (in_file filename);
              }
            with Found def_ident -> Project_def def_ident;
          end;
        with Not_found (* Misc.find_in_path_uncap *) -> Library_def
      end;
    | _ -> assert false
;;

(** find_project_external_references *)
let find_project_external_references =
  let find_project_external_references' project def =
    let def_modname = modname_of_path def.ident_loc.loc.loc_start.pos_fname in
    scan_project_files ~project begin fun acc filename entry ->
      try
        let mod_refs = Hashtbl.find_all entry.ext_refs def_modname in
        acc @ List.filter begin fun {ident_loc; _} ->
          (Longident.last (Longident.parse ident_loc.txt)) = def.ident_loc.txt
        end mod_refs
      with Not_found -> acc
    end;
  in
  fun ~project ~def ->
    match def.ident_kind with
      | Def d when d.def_scope.loc_end.pos_cnum = -1 -> find_project_external_references' project def
      | Def_module _ -> find_project_external_references' project def
      | Def_constr _ -> find_project_external_references' project def
      | Int_ref _ | Ext_ref | Open _ | Def _ -> []
;;

(** find_external_references *)
let find_external_references ~project ~ext_ref:{ident_kind; ident_loc; _} =
  match ident_kind with
    | Ext_ref | Open _ ->
      let lid = Longident.flatten (Longident.parse ident_loc.txt) in
      scan_project_files ~project begin fun acc filename entry ->
        match lid with
          | mod_name :: _ :: _ | mod_name :: [] ->
            let all_refs = Hashtbl.find_all entry.ext_refs mod_name in
            let refs = List.filter (fun x -> x.ident_loc.txt = ident_loc.txt) all_refs in
            acc @ refs;
          | _ -> assert false
      end
    | Def_constr _ | Def _  | Def_module _ | Int_ref _ -> []
;;

(** find_definition_and_references' *)
let find_definition_and_references' ~project ~entry ~ident =
  match ident.ident_kind with
    | Def _ | Def_constr _ -> (* Cursor offset is on a Def *)
      let int_refs = Hashtbl.find_all entry.int_refs ident.ident_loc.loc in
      let ext_refs = find_project_external_references ~project ~def:ident in
      {bai_def = Some ident; bai_refs = int_refs @ ext_refs}
    | Def_module def ->
      let int_refs = Hashtbl.find_all entry.int_refs ident.ident_loc.loc in
      let mod_refs =
        find_external_references ~project
          ~ext_ref:{ident_kind=Open none; ident_loc=mkloc def.def_name def.def_loc; ident_fname=""}
      in
      {bai_def = Some ident; bai_refs = int_refs @ mod_refs}
    | Int_ref def -> (* Cursor offset is on an Int_ref *)
      let bai_def = entry.locations.(def.loc_start.pos_cnum) in
      let int_refs = Hashtbl.find_all entry.int_refs def in
      let ext_refs = Opt.map_default bai_def [] (fun def -> find_project_external_references ~project ~def) in
      {bai_def; bai_refs = int_refs @ ext_refs}
    | (Ext_ref | Open _) as ext -> (* Cursor offset is on an Ext_ref *)
      begin
        match find_external_definition ~project ~ext_ref:ident with
          | Project_def def ->
            let entry_def = try Some (Hashtbl.find table_idents def.ident_fname) with Not_found -> None in
            let int_refs = Opt.map_default entry_def [] (fun e -> Hashtbl.find_all e.int_refs def.ident_loc.loc) in
            let ext_refs =
              if ext = Ext_ref
              then find_project_external_references ~project ~def
              else find_external_references ~project ~ext_ref:ident
            in
            {bai_def = Some def; bai_refs = int_refs @ ext_refs}
          | Library_def ->
            let refs = find_external_references ~project ~ext_ref:ident in
            {bai_def = None; bai_refs = refs}
          | Project_file fident ->
            let int_refs = [] in
            let ext_refs = find_external_references ~project ~ext_ref:ident in
            {bai_def = Some fident; bai_refs = int_refs @ ext_refs}
          | No_def ->
            let lib_refs = find_external_references ~project ~ext_ref:ident in
            {bai_def = None; bai_refs = lib_refs}
      end
;;

(** find_ident *)
let find_ident ~project ~filename ~offset ?compile_buffer () =
  scan ~project ~filename ?compile_buffer ();
  try
    let {locations; _} = Hashtbl.find table_idents filename in
    locations.(offset)
  with Not_found -> None
;;

(** find_local_definitions *)
let find_local_definitions ~project ~filename ?compile_buffer () =
  scan ~project ~filename ?compile_buffer ();
  try
    let {definitions; _} = Hashtbl.find table_idents filename in
    Some definitions
  with Not_found -> None
;;

(** find_definition_and_references *)
let find_definition_and_references ~project ~filename ~offset ?compile_buffer () =
  scan ~project ~filename ?compile_buffer ();
  try
    let entry = Hashtbl.find table_idents filename in
    let ident = find_ident ~project ~filename ~offset ?compile_buffer () in
    Opt.map_default ident None begin fun ident ->
      Some (find_definition_and_references' ~project ~entry ~ident)
    end;
  with Not_found -> None
;;

(** find_definition *)
let find_definition ~project ~filename ~offset ?compile_buffer () =
  scan ~project ~filename ?compile_buffer ();
  try
    let entry = Hashtbl.find table_idents filename in
    let ident = entry.locations.(offset) in
    Opt.map_default ident None begin fun ident ->
      match ident.ident_kind with
        | Def _ | Def_constr _ | Def_module _ -> Some ident
        | Int_ref def -> entry.locations.(def.loc_start.pos_cnum)
        | Ext_ref | Open _ ->
          begin
            match find_external_definition ~project ~ext_ref:ident with
              | Project_def def -> Some def
              | Project_file x -> Some x
              | Library_def -> None
              | No_def -> None
          end
    end;
  with Not_found -> None
;;

(** find_used_components *)
let find_used_components ~project ~filename ~offset ?compile_buffer () =
  scan ~project ~filename ?compile_buffer ();
  try
    let entry = Hashtbl.find table_idents filename in
    let ident = entry.locations.(offset) in
    Opt.map_default ident None begin fun ident ->
      match ident.ident_kind with
        | Open scope ->
          let longid = Longident.parse ident.ident_loc.txt in
          begin
            match Longident.flatten longid with
              | modname :: _ ->
                let uses = Hashtbl.find_all entry.ext_refs modname in
                let uses = List.filter begin function
                  | {ident_kind = Open _; _} -> false
                  | u when scope <==< u.ident_loc.loc.loc_start.pos_cnum ->
                    let dirname =
                      try
                        String.concat "." (List.rev (List.tl (List.rev (Longident.flatten (Longident.parse u.ident_loc.txt)))))
                      with Failure "tl" -> assert false
                    in
                    dirname = ident.ident_loc.txt
                  | _ -> false
                end uses in
                Some (ident, uses)
              | [] -> assert false
          end;
        | Def _ | Def_constr _ | Def_module _ | Int_ref _ | Ext_ref -> Some (ident, [])
    end;
  with Not_found -> None
;;



