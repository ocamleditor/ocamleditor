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

open Str
open Printf

let (//) = Filename.concat
let (!!) = Filename.dirname
let (!$) = Filename.quote
let (^^) = Filename.check_suffix

let finally = fun f1 f2 ->
  (** try ... finally ... *)
  try
    let result = Lazy.force f1 in
    Lazy.force f2;
    result
  with e -> (Lazy.force f2; raise e)

let (@$) = finally

let ( /* ) x f = f x and ( */ ) f x = f x;;

(** crono *)
let crono ?(label="Time") f x =
  let finally time =
    Printf.fprintf stdout "%s: %f sec." label (Unix.gettimeofday() -. time);
    print_newline();
  in
  let time = Unix.gettimeofday() in
  let result = try f x with e -> begin
    finally time;
    raise e
  end in
  finally time;
  result

(** {6 Operazioni su liste} *)

module Xlist =
  struct
    let rec rev_assoc x = function
      | [] -> raise Not_found
      | (a,b)::l -> if b = x then a else rev_assoc x l

    let pos x l =
      let rec f l n =
        match l with
          | [] -> raise Not_found
          | a :: b -> if x = a then n else f b (n + 1)
      in f l 0

    let remove_dupl l =
      List.rev (List.fold_left (fun acc y -> if List.mem y acc then acc else y :: acc) [] l)

    let filter_map p =
      let rec find accu = function
      | [] -> List.rev accu
      | x :: l ->
        begin
          match p x with
            | None -> find accu l
            | Some m -> find (m :: accu) l
        end
      in
      find [];;

    let max l =
      let rec find cand = function
      | [] -> cand
      | x :: l -> find (max x cand) l in
      match l with [] -> raise Not_found | x :: l -> find x l

    let list_full x n =
      let seq = ref [] in
      for i = 1 to n do seq := x :: !seq done;
      !seq;;

  end


(** {6 Memoization} *)

module Memo =
  struct
    let fast ~f =
      let memo = Hashtbl.create 1 in
      fun ?(force=fun _ -> false) key ->
        try
          let data = Hashtbl.find memo key in
          if force data then begin
            Hashtbl.remove memo key;
            raise Not_found;
          end;
          data
        with Not_found ->
          let data = f key in
          Hashtbl.add memo key data;
          data;;

    let sfast ~f =
      let memo = Hashtbl.create 1 in
      let count = ref 0 in
      let found = ref 0 in
      (fun ?(force=fun _ -> false) key ->
        incr count;
        let data = try
          let r = Hashtbl.find memo key in
          incr found;
          if force r then begin
            Hashtbl.remove memo key;
            raise Not_found;
          end;
          r
        with Not_found ->
          let data = f key in
          Hashtbl.add memo key data;
          data
        in
        data),
        (fun () ->
          Printf.fprintf stdout "%i/%i = %f" !found !count (float(!found)/.float(!count));
          print_newline()),
        (fun () ->
          Hashtbl.clear memo;
          count := 0;
          found := 0
        )
  end;;


(** [Str.regexp] with memoization *)
let (!~) = Memo.fast ~f:Str.regexp
let regexp = (!~)

(** Miscellaneus string functions *)

let split re = Str.split (!~ re)
let ltrim = replace_first (!~ "^[ \t\r\n]+") ""
let rtrim = replace_first (!~ "[ \t\r\n]+$") ""
let trim s = ltrim (rtrim s)
let trim_line = global_replace (!~ "[ \t]+\\($\\)") "\\1"

let lpad txt c width =
  let result = (String.make width c) ^ txt in
  String.sub result (String.length result - width) width;;

let rpad txt c width =
  let result = txt ^ (String.make width c) in
  String.sub result 0 width

(** Rimpiazza in un testo tutte le ricorrenze di sottostringhe con le relative
  stringhe sostitutive. L'ultima sottostringa viene rimpiazzata nel testo ottenuto dopo
  la sostituzione della penultima sottostringa, la penultima con la terzultima ecc.
  @param memo Indica se usare il memo per la compilazione delle espressioni
    regolari (default) o ricompilarle ogni volta.
  @param regexp Indica se la sottostringa da cercare è un'espressione regolare (default)
    o una stringa esatta.
*)
let replace_all ?(memo=true) =
  let mk_regexp = (!~) ~force:(fun _ -> not memo) in
  fun ?(regexp=true) re_te s ->
  List.fold_left begin fun s' (re, te) ->
    Str.global_replace (mk_regexp (if not regexp then Str.quote re else re)) te s'
  end s re_te

(** Simile a [replace_all] ma rimpiazza solo la prima ricorrenza. *)
let replace_first ?(memo=true) =
  let mk_regexp = (!~) ~force:(fun _ -> not memo) in
  fun ?(regexp=true) re_te s ->
  List.fold_left begin fun s' (re, te) ->
    Str.replace_first (mk_regexp (if not regexp then Str.quote re else re)) te s'
  end s re_te

(** [starts_with prefix s] restituisce [true] sse [s] inizia con [prefix]. *)
let starts_with prefix s = string_partial_match (regexp_string prefix) s 0


(** {6 Ricerca di espressioni regolari} *)

module Search =
  struct
    type 'a t =
      Skip (** Ignora la sotto-stringa trovata *)
      | Append of 'a (** Aggiunge la sottostringa trovata alla lista dei risultati *)
      | Found of 'a (** Restituisce la sottostringa trovata come unico
        risultato e interrompe la ricerca *)
    (** Rappresenta le istruzioni di controllo per le ricerche.
    *)

    let all pat f ?(pos=0) text  =
      let rec search pos acc =
        try
          let pos = search_forward pat text pos in
          let matched_string = matched_string text in
          let g = search (pos + 1) in
            (match f ~pos ~matched_string with
            | Skip -> g acc
            | Append x -> g (x :: acc)
            | Found x -> [x])
        with Not_found -> acc in
        List.rev (search pos [])
    (** Ricerca di tutte le sottostringhe di un testo. La funzione passata accetta come
       parametri la stringa trovata e la sua posizione e restituisce un'istruzione di
       controllo di tipo [Search.t]. Il risultato finale di [all] è la lista di tutti
       i risultati trovati ed elaborati.
    *)
  end

(** get_line_from_file *)
let get_line_from_file ~filename lnum =
  let chan = open_in filename in
  let result = ref "" in
  let i = ref 1 in
  try
    begin
      try
        while true do
          let line = input_line chan in
          if !i = lnum then (result := line; raise End_of_file);
          incr i;
        done;
      with End_of_file -> ()
    end;
    close_in chan;
    !result
  with ex -> (close_in chan; raise ex)

(** exec_lines *)
let exec_lines command =
  let ch = Unix.open_process_in command in
  set_binary_mode_in ch false;
  let result = ref [] in
  try
    while true do
      result := (input_line ch) :: !result;
    done;
    assert false
  with End_of_file -> begin
    ignore (Unix.close_process_in ch);
    List.rev !result
  end | e -> begin
    ignore (Unix.close_process_in ch);
    raise e
  end

(** expand *)
let expand ?(first_line=false) ?filter command =
  let ichan = Unix.open_process_in command in
  let finally () = ignore (Unix.close_process_in ichan) in
  let data = Buffer.create 100 in
  begin
    try
      while true do
        let line = trim (input_line ichan) in
        if first_line && String.length line = 0 then begin
        end else if first_line then begin
          Buffer.add_string data line;
          raise End_of_file
        end else begin
          match filter with
            | None ->
              Buffer.add_string data line;
              Buffer.add_char data '\n'
            | Some f when (f line) ->
              Buffer.add_string data line;
              Buffer.add_char data '\n'
            | _ -> ()
        end
      done
    with
      | End_of_file -> ()
      | ex -> (finally(); raise ex)
  end;
  finally();
  if Buffer.length data = 0 then (kprintf failwith "Miscellanea.expand: %s" command);
  Buffer.contents data;;

(** mkdir_p *)
let mkdir_p =
  let mkdir d = Unix.mkdir d 0o755 in
  fun d ->
    let rec loop d =
      if not (Sys.file_exists d) then begin
        loop (Filename.dirname d);
        mkdir d
      end
    in
    loop d

(** filename_split *)
(*let filename_split = Str.split (match Sys.os_type with "Win32" -> Str.regexp "[\\/]" | _ -> Str.regexp "/");;*)
let filename_split filename =
  let rec loop filename =
    let dirname = Filename.dirname filename in
    if dirname = "." || filename = dirname then [filename] else
      let basename = Filename.basename filename in
      basename :: (loop dirname)
  in List.rev (loop filename);;

(*
filename_split "a/b";;
filename_split "/a/b";;
filename_split "C:\\a\\b";;
*)

(** Restituisce la parte del percorso [path] relativa a [dirname].
  *  Per es. [filename_relative "/a/b" "/a/b/c/d" = "c/d"]
  *)
let filename_relative dirname =
  let dirname = filename_split dirname in
  fun path ->
    let path = filename_split path in
    let rec loop dirname path =
      match dirname, path with
        | [], path -> path
        | a :: b, x :: y when a = x -> loop b y
        | _ -> raise Not_found
    in try Some (List.fold_left Filename.concat "" (loop dirname path)) with Not_found -> None;;

(** filename_unix_implicit *)
let filename_unix_implicit filename =
  let parts = filename_split filename in
  let parts = if Filename.is_implicit filename then parts else (List.tl parts) in
  String.concat "/" parts
























