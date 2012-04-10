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

let table = Hashtbl.create 17
let table_critical = Mutex.create()
let itable = ref []

(** get_ref *)
let rec get_ref = function
  | [] -> None
  | x :: y -> (match x with Oe.Int_ref _ | Oe.Ext_ref _ -> Some x | _ -> get_ref y)

(** get_int_ref *)
let rec get_int_ref = function
  | [] -> None
  | x :: y -> (match x with Oe.Int_ref a -> Some a | _ -> get_int_ref y)

(** get_ext_ref *)
let rec get_ext_ref = function
  | [] -> None
  | x :: y -> (match x with Oe.Ext_ref a -> Some a | _ -> get_ext_ref y)

(** get_type *)
let rec get_type = function
  | [] -> None
  | x :: y ->
    begin
      match x with
        | Oe.Type descr -> Some descr
        | _ -> get_type y
    end

(** get_def *)
let rec get_def = function
  | [] -> None
  | x :: y ->
    begin
      match x with
        | Oe.Def a -> Some a
        | _ -> get_def y
    end

(** parse_file *)
let parse_file ~project ~filename ~ts =
  Mutex.lock table_critical;
  let name = "Parsing " ^ (Filename.basename filename) ^ "..." in
  GtkThread2.async (Activity.add Activity.Annot) name;
  begin
    try
      if Sys.file_exists filename then begin
        let ichan = open_in filename in
        let lexbuf = Lexing.from_channel ichan in
        try
          let blocks = Annot_parser.file Annot_lexer.token lexbuf in
          close_in ichan;
          let annot = { Oe.annot_blocks = blocks; annot_mtime = ts } in
          Hashtbl.replace table filename annot; (* basename.annot *)
          if List.length blocks > 0 then begin
            let s_filename = sprintf "%s.ml" (Filename.chop_extension filename) in
            itable := List.filter (fun ((f, _), _) -> f <> s_filename) !itable;
            List.iter begin fun block ->
              begin
                match get_def block.Oe.annot_annotations with
                  | None -> ()
                  | Some ((ident, _, stop) (*as scope*)) ->
                    itable := ((s_filename, ident), (stop, get_type block.Oe.annot_annotations)) :: !itable
              end;
  (*            begin
                match get_int_ref block.Oe.annot_annotations with
                  | None -> ()
                  | Some ((ident, start, _) as def) ->
                    itable := ((block.start.annot_fname, ident), block.start) :: !itable
              end;*)
            end blocks;
          end
        with ex -> (close_in ichan);
      end
    with Sys_error _ -> ()
  end;
  GtkThread2.async Activity.remove name;
  Mutex.unlock table_critical
;;

let (!!) filename = (Filename.chop_extension filename) ^ ".annot"

(** find *)
let rec find ~project ~filename () =
  if filename ^^ ".ml" then begin
    let fileannot = !! filename in
    try
      if Sys.file_exists filename then begin
        if Sys.file_exists fileannot then begin
          let amtime = (Unix.stat fileannot).Unix.st_mtime in
          let smtime = (Unix.stat filename).Unix.st_mtime in
          if smtime > amtime then (raise Not_found);
          let annot =
            try
              let ca = Hashtbl.find table fileannot in
              if ca.Oe.annot_mtime = amtime then ca
              else if ca.Oe.annot_mtime < amtime then (raise Not_found)
              else (raise Not_found)
            with Not_found -> begin
              parse_file ~project ~filename:fileannot ~ts:amtime;
              Hashtbl.find table fileannot;
            end
          in
          Some annot
        end else (raise Not_found)
      end else (raise Not_found)
    with Not_found -> begin
      Hashtbl.remove table fileannot;
      if Sys.file_exists fileannot then (try Sys.remove fileannot with Sys_error _ -> ());
      None
    end
  end else None

(** find_block_at_offset' *)
let find_block_at_offset' annot offset =
  List_opt.find begin fun block ->
    block.Oe.annot_start.Oe.annot_cnum <= offset && offset < block.Oe.annot_stop.Oe.annot_cnum
  end annot.Oe.annot_blocks;;

(** find_block_at_offset *)
let find_block_at_offset ~project ~filename ~offset =
  match find ~project ~filename () with
    | None -> None
    | Some annot -> find_block_at_offset' annot offset

(** preload *)
let preload ~project =
  let name = "Parsing \xC2\xAB.annot\xC2\xBB files..." in
  let finally () = GtkThread2.async Activity.remove name in
  ignore (Thread.create begin fun () ->
    GtkThread2.async (Activity.add Activity.Annot) name;
    try
      let src_path = Project.path_src project in
      let files = File.readdirs (*~links:false*) (Some (fun x -> x ^^ ".ml")) src_path in
      (*let files = List.filter (fun x -> x ^^ ".ml") files in*)
      List.iter (fun filename -> ignore (find ~project ~filename ())) files;
      finally()
    with ex -> begin
      Printf.eprintf "File \"annotation.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
      finally()
    end
  end ())
;;






