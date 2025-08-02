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

(** [Binannot_ident_scan.ext_def] copy *)
type ext_def = Binannot_ident_scan.ext_def =
  | Project_def of ident
  | Project_file of ident
  | Library_def
  | No_def


type search_result = {
  bai_def  : ident option;
  bai_refs : ident list;
}


module Log = Common.Log.Make(struct let prefix = "Binannot_ident" end)

(** find_external_definition *)
let find_external_definition ~project ~ext_ref =
  let longid = Binannot.longident_parse ext_ref.ident_loc.txt in
  match Longident.flatten longid with
  | name :: _ :: _ | name :: [] ->
      let ext_name = Longident.last longid in
      let longid = Odoc_misc.string_of_longident longid in
      let name = name ^ ".ml" in
      let paths = Project.get_search_path_local project in
      begin
        try
          let filename = Misc.find_in_path_uncap paths name in
          Binannot_ident_scan.scan ~project ~filename ();
          let {locations; _} = Hashtbl.find Binannot.table_idents filename in
          begin
            try
              for i = Array.length locations - 1 downto 0 do
                match locations.(i) with
                | None -> ()
                | Some ({ident_kind; ident_loc; _} as def_ident) ->
                    match ident_kind with
                    | Def def
                      when def.def_scope.loc_end.pos_cnum = -1 && ident_loc.txt = ext_name ->
                        raise (Binannot_ident_scan.Found def_ident)
                    | Def_constr def
                      when def.def_name = ext_name -> raise (Binannot_ident_scan.Found def_ident)
                    | Def_module def
                      when def.def_name = longid -> raise (Binannot_ident_scan.Found def_ident)
                    | Int_ref _ | Ext_ref | Open _ | Def _ | Def_constr _ | Def_module _ -> ()
              done;
              (* If ident is not found in "locations", return an ident corresponding to the file *)
              Project_file {
                ident_fname  = filename;
                ident_kind   = Def {def_name=ext_ref.ident_loc.txt; def_loc=Location.none; def_scope=Location.none};
                ident_loc    = Location.mkloc ext_ref.ident_loc.txt (Location.in_file filename);
              }
            with Binannot_ident_scan.Found def_ident -> Project_def def_ident;
          end;
        with Not_found (* Misc.find_in_path_uncap *) -> Library_def
      end;
  | _ -> assert false
;;

(** find_project_external_references *)
let find_project_external_references =
  let find_project_external_references' project def =
    let { Asttypes.loc; _ } = def.ident_loc in
    let def_modname = modname_of_path loc.loc_start.pos_fname in
    Binannot_ident_scan.scan_project_files ~project begin fun acc _filename entry ->
      try
        let mod_refs = Hashtbl.find_all entry.ext_refs def_modname in
        let filtered =
          List.filter begin fun {ident_loc; _} ->
            (Longident.last (Binannot.longident_parse ident_loc.txt)) = def.ident_loc.txt
          end mod_refs
        in
        acc @ filtered
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
      let lid = Longident.flatten (Binannot.longident_parse ident_loc.txt) in
      Binannot_ident_scan.scan_project_files ~project begin fun acc _filename entry ->
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
  let { Asttypes.loc; _ } = ident.ident_loc in
  match ident.ident_kind with
  | Def _ | Def_constr _ -> (* Cursor offset is on a Def *)
      let int_refs = Hashtbl.find_all entry.int_refs loc in
      let ext_refs = find_project_external_references ~project ~def:ident in
      {bai_def = Some ident; bai_refs = int_refs @ ext_refs}
  | Def_module def ->
      let int_refs = Hashtbl.find_all entry.int_refs loc in
      let mod_refs =
        find_external_references ~project
          ~ext_ref:{ident_kind=Open Location.none; ident_loc=Location.mkloc def.def_name def.def_loc; ident_fname=""}
      in
      {bai_def = Some ident; bai_refs = int_refs @ mod_refs}
  | Int_ref def -> (* Cursor offset is on an Int_ref *)
      let bai_def = entry.locations.(def.loc_start.pos_cnum) in
      let int_refs = Hashtbl.find_all entry.int_refs def in
      let ext_refs = Option.fold ~none:[] ~some:(fun def -> find_project_external_references ~project ~def) bai_def in
      {bai_def; bai_refs = int_refs @ ext_refs}
  | (Ext_ref | Open _) as ext -> (* Cursor offset is on an Ext_ref *)
      begin
        match find_external_definition ~project ~ext_ref:ident with
        | Project_def def ->
            let { Asttypes.loc; _ } = def.ident_loc in
            let entry_def = try Some (Hashtbl.find Binannot.table_idents def.ident_fname) with Not_found -> None in
            let int_refs = Option.fold ~none:[] ~some:(fun e -> Hashtbl.find_all e.int_refs loc) entry_def in
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
  Binannot_ident_scan.scan ~project ~filename ?compile_buffer ();
  try
    let {locations; _} = Hashtbl.find Binannot.table_idents filename in
    locations.(offset)
  with Not_found -> None
;;

(** find_local_definitions *)
let find_local_definitions ~project ~filename ?compile_buffer () =
  Binannot_ident_scan.scan ~project ~filename ?compile_buffer ();
  try
    let {definitions; _} = Hashtbl.find Binannot.table_idents filename in
    Some definitions
  with Not_found -> None
;;

(** find_definition_and_references *)
let find_definition_and_references ~project ~filename ~offset ?compile_buffer () =
  Binannot_ident_scan.scan ~project ~filename ?compile_buffer ();
  try
    let entry = Hashtbl.find Binannot.table_idents filename in
    let ident = find_ident ~project ~filename ~offset ?compile_buffer () in
    Option.fold ~none:None ~some:(fun ident ->
        Some (find_definition_and_references' ~project ~entry ~ident)) ident;
  with Not_found -> None
;;

(** find_definition *)
let find_definition ~project ~filename ~offset ?compile_buffer () =
  Binannot_ident_scan.scan ~project ~filename ?compile_buffer ();
  try
    let entry = Hashtbl.find Binannot.table_idents filename in
    let ident = try entry.locations.(offset)
      with Invalid_argument _ ->
        Log.println `ERROR "Invalid offset %d for %s" offset filename;
        None
    in
    Option.fold ~none:None ~some:(fun ident ->
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
            end) ident;
  with Not_found -> None
;;

(** find_used_components *)
let find_used_components ~project ~filename ~offset ?compile_buffer () =
  Binannot_ident_scan.scan ~project ~filename ?compile_buffer ();
  try
    let ( <==< ) = Binannot.( <==< ) in
    let entry = Hashtbl.find Binannot.table_idents filename in
    let ident = entry.locations.(offset) in
    Option.fold ident ~none:None ~some:begin fun ident ->
      match ident.ident_kind with
      | Open scope ->
          let { Asttypes.txt; _ } = ident.ident_loc in
          let longid = Binannot.longident_parse txt in
          begin
            match Longident.flatten longid with
            | modname :: _ ->
                let uses = Hashtbl.find_all entry.ext_refs modname in
                let uses = List.filter begin function [@warning "-4"]
                  | {ident_kind = Open _; _} -> false
                  | u when scope <==< u.ident_loc.Asttypes.loc.loc_start.pos_cnum ->
                      let dirname =
                        try
                          String.concat "." (List.rev (List.tl (List.rev (Longident.flatten (Binannot.longident_parse u.ident_loc.txt)))))
                        with Failure _ -> assert false
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



