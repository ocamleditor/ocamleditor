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
open Preferences

module Log = Common.Log.Make(struct let prefix = "Binannot" end)
let _ = Log.set_verbosity `ERROR

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

type definition = {
  mutable def_name  : string;
  mutable def_loc   : location;
  mutable def_scope : location;
}

type ident_kind =
  | Def of definition
  | Def_constr of definition
  | Def_module of definition
  | Int_ref of location (* Location of its defintion *)
  | Ext_ref
  | Open of location (* Scope *)

type ident = {
  mutable ident_fname : string; (* Filename *)
  mutable ident_kind  : ident_kind;
  ident_loc           : string loc; (* Name of the ident and location *)
}

type entry = { (* An entry collects all ident annotations of a source file. *)
  timestamp           : float; (* mtime of the cmt file read *)
  locations           : ident option array; (* Map from file offsets to idents *)
  int_refs            : (location, ident) Hashtbl.t; (* Map from def's locations to all its internal refs. *)
  ext_refs            : (string, ident) Hashtbl.t; (* Map from module names (compilation units) to all external references *)
  mutable definitions : definition list; (* List of all definitions in the file *)
}

module Longident = struct
  include Longident

  (*
    parse "A.B.c";;
    - : t = Ldot (Ldot (Lident "A", "B"), "c")
    flatten (parse "A.B.c");;
    - : string list = ["A"; "B"; "c"]
    last (parse "A.B.c");;
    - : string = "c"
  *)

  let qualified = function
    | Lident _ -> false
    | Ldot _ -> true
    | Lapply _ -> true

  let of_type_expr typ name =
    match typ with
    | Ldot (path, _) -> Ldot (path, name)
    | Lapply (path, _) -> Ldot (path, name)
    | Lident _ -> Longident.Lident name

end

(** [location] formatter *)
let pp_loc ppf loc =
  if loc = Location.none then
    Format.fprintf ppf "_none_"
  else
    let { loc_start; loc_end; _ } = loc in
    let line  = loc_start.pos_lnum in
    let start = loc_start.pos_cnum - loc_start.pos_bol in
    let stop  = loc_end.pos_cnum - loc_end.pos_bol in
    if loc_end.pos_lnum  = line then
      Format.fprintf ppf "%s: %d[%d+%d]"
        loc_start.pos_fname line start (stop - start)
    else
      Format.fprintf ppf "%s: %d[%d]..%d[%d]"
        loc_start.pos_fname line start loc_end.pos_lnum stop

(** [location] alternative formatter without the file name *)
let pp_short_loc ppf loc =
  if loc = Location.none then
    Format.fprintf ppf "_none_"
  else
    let { loc_start; loc_end; _ } = loc in
    let line  = loc_start.pos_lnum in
    let start = loc_start.pos_cnum - loc_start.pos_bol in
    let stop  = loc_end.pos_cnum - loc_end.pos_bol in
    if loc_end.pos_lnum = line then
      Format.fprintf ppf "%d[%d+%d]" line start (stop - start)
    else
      Format.fprintf ppf "%d[%d]..%d[%d]" line start loc_end.pos_lnum stop


(** [definition] formatter *)
let pp_definition ppf { def_name; def_loc; def_scope } =
  Format.fprintf ppf "%s: %a, scope:%a" def_name
    pp_short_loc def_loc pp_loc def_scope

(** [ident] formatter *)
let pp_ident ppf { ident_kind; ident_loc; _ } =
  match ident_kind with
  | Def def        -> Format.fprintf ppf "var %a, start %d" pp_definition def def.def_loc.loc_start.pos_cnum
  | Def_constr def -> Format.fprintf ppf "ctr %a, start %d" pp_definition def def.def_loc.loc_start.pos_cnum
  | Def_module def -> Format.fprintf ppf "mod %a, start %d" pp_definition def def.def_loc.loc_start.pos_cnum
  | Int_ref loc    -> Format.fprintf ppf "ref %s:%a defined at: %a"
                        ident_loc.txt pp_short_loc ident_loc.loc pp_loc loc
  | Ext_ref        -> Format.fprintf ppf "ext %s:%a" ident_loc.txt
                        pp_short_loc ident_loc.loc
  | Open loc       -> Format.fprintf ppf "opn %a" pp_loc loc

let longident_parse repr = try
    Longident.parse repr
  with ex ->
    (* Just a stopgap measure. The plan is to get rid of [Longident.parse] altogether *)
    Log.println `ERROR " !! unable to parse Longident.t from: %s" repr;
    raise ex

let table_idents : (string, entry) Hashtbl.t = Hashtbl.create 7 (* source filename, entry *)

let string_of_kind = function
  | Def _ -> "Def"
  | Def_module _ -> "Def_module"
  | Def_constr _ -> "Def_constr"
  | Int_ref _ -> "Int_ref"
  | Ext_ref -> "Ext_ref"
  | Open _ -> "Open"

let string_of_loc loc =
  let filename, _, _ = Location.get_pos_info loc.loc_start in
  Format.sprintf "%s:%d--%d" filename (loc.loc_start.pos_cnum) (loc.loc_end.pos_cnum);;

let linechar_of_loc loc =
  let _, a, b = Location.get_pos_info loc.loc_start in
  let _, c, d = Location.get_pos_info loc.loc_end in
  ((a - 1), b), ((c - 1), d)

let cnum_of_loc loc =
  loc.loc_start.pos_fname, loc.loc_start.pos_cnum, loc.loc_end.pos_cnum

let string_of_type_expr te = Odoc_info.string_of_type_expr te;;
let lid_of_type_expr te = longident_parse (string_of_type_expr te)

let (<==) loc offset = loc.loc_start.pos_cnum <= offset && offset <= loc.loc_end.pos_cnum
let (<==<) loc offset = loc.loc_start.pos_cnum <= offset && (offset <= loc.loc_end.pos_cnum || loc.loc_end.pos_cnum = -1)

(** read_cmt *)
let read_cmt ~project ~filename:source ?(timestamp=0.) ?compile_buffer () =
  try
    let result =
      let ext = if source ^^^ ".ml" then Some ".cmt" else if source ^^^ ".mli" then Some ".cmti" else None in
      match ext with
      | Some ext ->
          Option.fold (Project.tmp_of_abs project source) ~none:None ~some:(fun (tmp, relname) ->
              let source_tmp = tmp // relname in
              let source_avail =
                if Sys.file_exists source_tmp then source_tmp
                else Option.fold compile_buffer ~none:source ~some:(fun f -> f (); source_tmp)
              in
              let cmt = (Filename.chop_extension source_avail) ^ ext in
              let mtime_cmt = (Unix.stat cmt).Unix.st_mtime in
              if mtime_cmt = timestamp then None else begin
                let mtime_src = (Unix.stat source_avail).Unix.st_mtime in
                if mtime_cmt >= mtime_src then begin
                  Some (source, mtime_cmt, Cmt_format.read_cmt cmt)
                end else None
              end
            )
      | _ -> None (*kprintf invalid_arg "Binannot.read_cmt \"%s\"" source*)
    in
    result
  with
  | Unix.Unix_error (Unix.ENOENT as err, "stat", _) as ex ->
      Log.println `WARN "%s - %s" (Printexc.to_string ex) (Unix.error_message err);
      None
  | Unix.Unix_error (err, _, _) as ex ->
      Log.println `ERROR "%s - %s" (Printexc.to_string ex) (Unix.error_message err);
      None
  | ex ->
      Log.println `ERROR "%s" (Printexc.to_string ex);
      None
;;

(** print_ident *)
let print_ident ?filter {ident_kind; ident_loc; _} =
  let loc' =
    match ident_kind with
    | Def loc -> "scope: " ^ (string_of_loc loc.def_scope)
    | Def_module def -> "def: " ^ (string_of_loc def.def_loc)
    | Def_constr def -> "def: " ^ (string_of_loc def.def_loc)
    | Int_ref def_loc -> "def: " ^ (string_of_loc def_loc)
    | Ext_ref -> "---"
    | Open scope -> "scope: " ^ (string_of_loc scope)
  in
  let { txt; loc } = ident_loc in
  match filter with
  | Some x when x <> txt -> ()
  | _ ->
      Format.printf "%-11s: %-30s (use: %-12s) (%-19s) %s\n%!"
        (String.uppercase_ascii (string_of_kind ident_kind))
        ident_loc.txt
        (string_of_loc loc)
        loc'
        loc.loc_start.pos_fname;
;;





