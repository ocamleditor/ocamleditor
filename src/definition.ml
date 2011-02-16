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

open Printf
open Annotation
open Annot_types
open Miscellanea

type references = {
  ref_def : ref_pos;
  ref_def_scope : (position * position option) option;
  ref_int : ref_pos list;
  ref_ext : (string * ref_pos list) list; (* filename * ... *)
}
and ref_pos = string * (int * int) * (int * int)
(* name * (start_nline * start_line_offset) * (stop_nline * stop_line_offset) *)

(** find_def *)
let find_def ~annot ~name =
  List.fold_left begin fun acc block ->
    match Annotation.get_def block.annotations with
      | Some ((n, _, _) as d) when n = name -> (block, d) :: acc
      | _ -> acc
  end [] annot.blocks;;

(** find_ext_ref *)
let find_ext_ref ~src_path from =
  let comp = match from with
    | `MATCH (def_filename, def_name) ->
      let mod_name = String.capitalize (Filename.chop_extension (Filename.basename def_filename)) in
      let re = Str.regexp_case_fold (sprintf "%s\\.\\([A-Z][A-Za-z_0-9']*\\.\\)*%s$" (Str.quote mod_name) (Str.quote def_name)) in
      fun name -> Str.string_match re name 0
    | `EXACT fullname -> (=) fullname
  in
  let ext_references : (string * ref_pos list) list ref = ref [] in
  let file_annots = File.readdirs ~links:false src_path in
  let file_annots = List.filter (fun x -> x ^^ ".ml") file_annots in
  List.iter begin fun filename ->
    let current_file_refs = ref [] in
    begin
      match find ~filename with
        | None -> ()
        | Some annots ->
          List.iter begin fun block ->
            match get_ext_ref block.annotations with
              | Some ext when (comp ext) ->
                current_file_refs := (Filename.basename filename,
                  (block.start.pos_lnum, block.start.pos_cnum - block.start.pos_bol),
                  (block.stop.pos_lnum, block.stop.pos_cnum - block.stop.pos_bol)) :: !current_file_refs
              | _ -> ()
          end annots.blocks;
    end;
    if !current_file_refs <> [] then
      ext_references := (filename, (List.rev !current_file_refs)) :: !ext_references;
  end file_annots;
  List.sort (fun (x, _) (y, _) -> Pervasives.compare x y) !ext_references;;

(** find_references *)
let find_references ?src_path ~filename ~offset (* offset of a definition (def) or let...in *) () =
  match find ~filename with
    | None -> None
    | Some annots ->
      begin
        match find_block_at_offset' annots offset with
          | None -> None
          | Some block_def ->
            let start_def = block_def.start.pos_cnum in
            (* Internal references *)
            let irefs = Xlist.filter_map begin fun block ->
              match get_int_ref block.annotations with
                | Some (n, x, _) when x.pos_cnum = start_def ->
                  Some (n,
                    (block.start.pos_lnum, block.start.pos_cnum - block.start.pos_bol),
                    (block.stop.pos_lnum, block.stop.pos_cnum - block.stop.pos_bol))
                | _ -> None
            end annots.blocks in
            (* Informations about the definition *)
            let a, b, c, d =
              block_def.start.pos_lnum,
              block_def.start.pos_cnum - block_def.start.pos_bol,
              block_def.stop.pos_lnum,
              block_def.stop.pos_cnum - block_def.stop.pos_bol
            in
            let def_name = match Annotation.get_def block_def.annotations with
              | None -> (* let...in definition without "def" in .annot file *)
                (try let (name, _, _) = List.hd irefs in name with Failure "hd" -> "_unused_")
              | Some (x, _, _) -> x
            in
            let def = def_name, (a, b), (c, d) in
            (* External references *)
            let ext_references = match src_path with
              | None -> []
              | Some src_path ->
                find_ext_ref ~src_path (`MATCH (filename, def_name))
            in
            (*  *)
            Some {ref_def = def; ref_def_scope = None; ref_int = irefs; ref_ext = ext_references}
      end;;

(** find_definition *)
let find_definition ~project ~page  ~(iter : GText.iter) =
  match page#buffer#get_annot iter with
    | None -> None
    | Some { Annot_types.annotations = annotations; start=block_start; stop=block_stop } ->
      let block_start = block_start.Annot_types.pos_cnum in
      let block_stop = block_stop.Annot_types.pos_cnum in
      begin
        match Annotation.get_ref annotations with
        | Some (Annot_types.Int_ref (_ , start, stop)) ->
          (* The block is an internal reference *)
          let start = start.Annot_types.pos_cnum in
          let stop = stop.Annot_types.pos_cnum in
          Some (block_start, block_stop, page#get_filename, start, stop)
        | Some (Annot_types.Ext_ref name) ->
          (* The block is an external reference *)
          begin
            try
              let lident = (Longident.flatten (Longident.parse name)) in
              let filename = (List.hd lident) ^ ".ml" in
              let fullname = Misc.find_in_path_uncap project.Project.source_paths filename in
              let filename =
                match project.Project.in_source_path fullname with
                  | Some x -> x | _ -> assert false
              in
              let nested = List.length lident > 2 in
              let ident = List.hd (List.rev lident) in
              begin
                match Annotation.find ~filename with
                  | None -> None
                  | Some annot ->
                    let defs = find_def ~annot ~name:ident in
                    let def =
                      if nested then List.hd defs else (List.find (fun (_, (_, _, stop)) -> stop = None) defs)
                    in
                    let block, _ = def in
                    Some (block_start, block_stop, fullname,
                      block.Annot_types.start.Annot_types.pos_cnum, block.Annot_types.stop.Annot_types.pos_cnum)
              end
            with Not_found | Failure "hd" -> None
          end
        | _ ->
          begin (* The block where iter is placed is a definition (def) or a let...in *)
            match Annotation.get_def annotations with
              | None -> (* let...in *)
                let offset = Glib.Utf8.offset_to_pos
                  (page#view#buffer#get_text ?start:None ?stop:None ?slice:None ?visible:None ()) ~pos:0 ~off:iter#offset
                in
                begin
                 match find_references ~filename:page#get_filename ~offset () with
                   | Some references when references.ref_int <> [] ->
                     Some (block_start, block_stop, page#get_filename, block_start, block_stop)
                   | _ -> None
                end;
              | Some (_, start, stop) ->
                Some (block_start, block_stop, page#get_filename, block_start, block_stop)
          end
      end;;













