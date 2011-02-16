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

open Miscellanea
open Unix
open Str
open Printf

module Util =
struct
  let is_directory name =
     try (Unix.stat name).Unix.st_kind = Unix.S_DIR with _ -> false;;

  let get_directories_in_files path =
  (*  let path = if is_directory path then path else Filename.dirname path in*)
    let entries = Array.to_list (Sys.readdir path) in
    List.filter (fun e -> is_directory (Filename.concat path e)) entries;;

  let get_files_in_directory path =
  (*  let path = if is_directory path then path else Filename.dirname path in*)
    let entries = Array.to_list (Sys.readdir path) in
    List.filter (fun e -> not (is_directory (Filename.concat path e))) entries

  let get_directories_and_files ?(directories_first=false) path =
    let entries = Array.to_list (Sys.readdir path) in
    if directories_first then
      List.sort (fun e1 e2 -> if is_directory (Filename.concat path e1) then -1 else 0) entries
    else entries;;

  (**** Memoized rexgexp *)

  let (~!) = Miscellanea.Memo.fast ~f:Str.regexp;;

  (************************************************************ Path name *)

  (* Convert Windows-style directory separator '\' to caml-style '/' *)
  let caml_dir path =
    if Sys.os_type = "Win32" then
      global_replace ~!"\\\\" "/" path
    else path

  let rec fixpoint ~f v =
    let v' = f v in
    if v = v' then v else fixpoint ~f v';;

  let unix_regexp s =
    let s = Str.global_replace ~!"\\*" ".*" s in
    let s = Str.global_replace ~!"\\?" ".?" s in
    let s =
      fixpoint s
        ~f:(Str.replace_first ~!"\\({.*\\),\\(.*}\\)" "\\1\\|\\2") in
    let s =
      Str.global_replace ~!"{\\(.*\\)}" "\\(\\1\\)" s in
    Str.regexp s;;

  let exact_match ~pat s =
    Str.string_match pat s 0 && Str.match_end () = String.length s;;

  let ls ~dir ~pattern =
    let files = get_directories_and_files dir in
    let regexp = unix_regexp pattern in
    List.filter (exact_match ~pat:regexp) files;;

  let lsd ~dir ~pattern =
    let directories = get_directories_in_files dir in
    let regexp = unix_regexp pattern in
    List.filter (exact_match ~pat:regexp) directories

  let mtime filename = (Unix.stat filename).st_mtime

  let format_time time =
    let t = Unix.localtime time in
    Printf.sprintf "%i/%i/%i %02i:%02i" t.tm_mday (t.tm_mon + 1) (t.tm_year + 1900)
      t.tm_hour t.tm_min

  let crono ?(label="Time") f x =
    let finally time =
      Printf.fprintf Pervasives.stdout "%s: %f sec." label (Unix.gettimeofday() -. time);
      print_newline();
    in
    let time = Unix.gettimeofday() in
    let result = try f x with e -> begin
      finally time;
      raise e
    end in
    finally time;
    result
end;;


let read filename =
  let ch = open_in_bin filename in
  lazy begin
    let len = in_channel_length ch in
    let buf = Buffer.create len in
    Buffer.add_channel buf ch len;
    buf
  end /*finally*/ lazy (close_in ch)

let write filename text =
  let ch = open_out_bin filename in
  lazy begin
    output_string ch text;
  end /*finally*/ lazy (close_out ch)

(** Copy file *)
let copy_file ic oc =
  let buff = String.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let cp ?(echo=false) src dst =
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  if echo then (printf "%s -> %s\n%!" src dst);
  let finally () = close_out oc; close_in ic in
  try copy_file ic oc; finally() with ex -> (finally(); raise ex)

(** readdirs *)
let rec readdirs ?(links=false) path =
  try
    if not links && (Unix.lstat path).Unix.st_kind = Unix.S_LNK then []
    else if not (Sys.is_directory path) then [path] else begin
      let files = Sys.readdir path in
      let files = List.map (Filename.concat path) (Array.to_list files) in
      List.flatten (List.map (readdirs ~links:false) files)
    end
  with Sys_error _ -> [];;

(** readtree *)
let rec readtree path =
  try
    if not (Sys.is_directory path) then [path] else begin
      let files = Sys.readdir path in
      let dirs = Miscellanea.Xlist.filter_map begin fun x ->
        let filename = Filename.concat path x in
        if Sys.is_directory filename then (Some filename) else None
      end (Array.to_list files) in
      path :: (List.flatten (List.map readtree dirs))
    end
  with Sys_error _ -> [];;

(** Operazioni su file
*)
class file path name =
  object (self)
    val mutable ts = (stat (Filename.concat path name)).st_mtime
    val mutable dirname = path
    val mutable basename = name
    (** L'ultimo accesso al file (#read o #write) o creazione dell'oggetto.
      Solleva Unix_error se il file non esiste.
    *)
    method path = Filename.concat dirname basename
    method name = basename
    method last_modified () = (stat self#path).st_mtime
    method is_readonly =
      let exists, read, write, execute = self#perm in
      exists && read && not write
    (** Se il file è cambiato sul disco dall'ultima operazione di
      creazione, lettura o scrittuta *)
    method changed = ts < self#last_modified()
    method read =
      self#update_ts ();
      Buffer.contents (read self#path)
    method write text =
      write self#path text;
      self#update_ts ();
    method private perm =
      let exists = try access self#path [F_OK]; true with Unix_error _ -> false in
      let read = try access self#path [R_OK]; true with Unix_error _ -> false in
      let write = try access self#path [W_OK]; true with Unix_error _ -> false in
      let execute = (*try access self#path [X_OK]; true with Unix_error _ -> false*) read in
      exists, read, write, execute
    method is_writable = match self#perm with _, _, x, _ -> x
    method private update_ts () = ts <- self#last_modified()

    (** Crea una copia di backup del file aggiungendo al nome il suffisso [.~n~], dove l'indice [n] è il
        massimo più uno tra gli indici delle altre copie di backup del file che già
        esistono. Se non esiste nessuna copia di backup precedente, il primo indice è 1.
        Restituisce il nome del file creato. Se il file non esiste non viene fatta nessuna copia e
        restituisce [""].
        @param move_to Se specificato crea il backup in questa directory e se non esiste la crea. *)
    method backup ?move_to () =
      if Sys.file_exists self#path then begin
        let dir = match move_to with None -> self#path
          | Some path ->  
            if not (Sys.file_exists path) then begin
              let dirs = filename_split path in
              ignore (List.fold_left begin fun acc dir ->
                let dir = Filename.concat acc dir in
                if not (Sys.file_exists dir) then (printf "%s, %s\n%!" path dir; Unix.mkdir dir 0o777);
                dir
              end "" dirs);
            end;
            path
        in
        let name_match = Str.string_match (Str.regexp (name ^ "\\.~\\([0-9]+\\)~$")) in
        let backups = Xlist.filter_map begin fun n ->
          if name_match n 0 then
            Some (int_of_string (Str.matched_group 1 n))
          else None
        end (Array.to_list (Sys.readdir dir)) in
        let n = try Xlist.max backups + 1 with Not_found -> 1 in
        let backup_name = self#path ^ ".~" ^ (string_of_int n) ^ "~" in
        let backup_name = match move_to with None -> backup_name
          | Some path -> Filename.concat path (Filename.basename backup_name)
        in
        cp self#path backup_name;
        backup_name
      end else ""

    method rename newname =
      Sys.rename self#path newname;
      dirname <- Filename.dirname newname;
      basename <- Filename.basename newname;
  end


(** @param parent Percorso della directory
    @param filename Nome del file; se [parent] non è specificato, il percorso della
      directory viene cercato qui. *)
let create ?parent filename () =
  let p, n =
    match parent with
      | None -> Filename.dirname filename, Filename.basename filename
      | Some p -> p, filename in
  let file = new file p n in
  file
