(*

  OCamlEditor
  Copyright (C) 2010-2013 Francesco Tovagliari

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

let _ = Curl.global_init Curl.CURLINIT_GLOBALALL

let re_space = Str.regexp " "
let re_spaces = Str.regexp " +"
let re_cap = Str.regexp "^"
let re_colon = Str.regexp ":"

let month_of_string = function
  | "Jan" -> 0
  | "Feb" -> 1
  | "Mar" -> 2
  | "Apr" -> 3
  | "May" -> 4
  | "Jun" -> 5
  | "Jul" -> 6
  | "Aug" -> 7
  | "Sep" -> 8
  | "Oct" -> 9
  | "Nov" -> 10
  | "Dec" -> 11
  | _ -> assert false

module Remote = struct

  type stats = {
    perm : string;
    size : int;
    mtime : float;
  }

  (** file *)
  class file ~host ~user ~pwd ~filename : Editor_file_type.abstract_file =
    object (self)

    val dirname = Filename.dirname filename
    val connection =
        let c = Curl.init () in
        Gc.finalise Curl.cleanup c;
        c
    val mutable mtime = 0.0

    initializer
      match self#stat () with
        | Some perm -> mtime <- perm.mtime;
        | _ -> ()

    method filename = filename
    method basename = Filename.basename filename

    method private get_url () = sprintf "sftp://%s:%s@%s%s" user pwd host filename

    method read =
      let buf = Buffer.create 100 in
      let url = self#get_url () in
      Curl.set_url connection url;
      (*Curl.set_ftplistonly connection true;*)
      Curl.set_writefunction connection begin fun str ->
        Buffer.add_string buf str;
        String.length str;
      end;
      Curl.perform connection;
      Buffer.contents buf;

    method write str =
      try
        let url = self#get_url () in
        Curl.set_url connection url;
        Curl.set_upload connection true;
        let buf = ref str in
        Curl.set_readfunction connection begin fun max_bytes ->
          if !buf = "" then "" else
            let blen = String.length !buf in
            let len = min max_bytes blen in
            let bytes = String.sub !buf 0 len in
            buf := String.sub !buf len (blen - len);
            bytes
        end;
        Curl.perform connection;
        mtime <- Unix.gettimeofday();
        ignore (self#stat());
      with Curl.CurlException (code, n, s) ->
        Printf.eprintf "%d - %s - %s\n%!" n (Curl.strerror code) s;

    method private get_filetime () =
      let url = self#get_url () in
      Curl.set_url connection url;
      let tm = Curl.get_filetime connection in
      Curl.perform connection;
      tm

  method private perm_of_string str =
    assert (String.length str = 10);
    0

  method private time_of_string str =
    let open Unix in
    match Str.split re_spaces str with
      | [mon; day; year_time] ->
        let ltm = Unix.localtime (Unix.gettimeofday ()) in
        let year, hour, min =
          if String.contains year_time ':' then
            let time = Str.split re_colon year_time in
            let year = ltm.tm_year in
            begin
              match time with
                | [h; m] ->
                  year,
                  int_of_string h,
                  int_of_string m
                | _ -> assert false
            end
          else (int_of_string year_time) - 1900, 0, 0
        in
        mktime {
          tm_sec = 0;
          tm_min = min;
          tm_hour = hour;
          tm_mday = int_of_string day;
          tm_mon = month_of_string mon;
          tm_year = year;
          tm_wday = 0;
          tm_yday = 0;
          tm_isdst = ltm.tm_isdst;
        }
      | _ -> assert false

   method private stat () =
      let url = sprintf "sftp://%s:%s@%s%s/" user pwd host dirname in
      Curl.set_url connection url;
      Curl.set_upload connection false;
      let buf = Buffer.create 1000 in
      Curl.set_writefunction connection begin fun str ->
        Buffer.add_string buf str;
        String.length str;
      end;
      Curl.perform connection;
      let listing = Buffer.contents buf in
      let re_filename = kprintf Str.regexp " %s$" self#basename in
      try
        let stop = Str.search_forward re_filename listing 0 in
        let start = Str.search_backward re_cap listing stop in
        let line = String.sub listing start (stop - start) in
        let perm = String.sub line 0 10 in (* -rwxr--r-x *)
        let time = String.sub line (String.length line - 12) 12 in (* 1554 Jan  7 18:41 *)
        let stop = String.length line - 12 - 2 in
        let start = Str.search_backward re_space line stop in
        let size = int_of_string (String.sub line (start + 1) (stop - start)) in
        let ftime, time = self#time_of_string time in
        (*Printf.printf "%S -- %S -- (%d/%d/%d %d:%d) -- %d\n%!" line perm
          (time.Unix.tm_year + 1900)
          (time.Unix.tm_mon + 1)
          time.Unix.tm_mday
          time.Unix.tm_hour
          time.Unix.tm_min
          size;*)
        Some {perm; size; mtime=ftime};
      with Not_found -> None

    method is_readonly =
      match self#stat() with
        | Some stat ->
          let perm = stat.perm in
          perm.[1] = 'r' && perm.[2] = '-'
        | _ -> true

    method is_writeable =
      match self#stat() with
        | Some stat ->
          let perm = stat.perm in
          perm.[1] = 'r' && perm.[2] = 'w'
        | _ -> false

    method last_modified () =
      match self#stat() with
        | Some stat -> stat.mtime
        | _ -> 0.0

    method changed = mtime < self#last_modified()

    method backup ?move_to () =
      Printf.eprintf "[Remote] backup not implemented\n%!" ;
      ""

    method rename newname =
      failwith "rename not implemented"

    method is_remote = true

    end

  let create = new file

end

let _ = Plugins.remote := Some (module Remote : Plugins.REMOTE)

