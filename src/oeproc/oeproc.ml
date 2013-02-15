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

let main () =
  Random.self_init();
  let port = 50000 + Random.int 1000 in
  let addr = (Unix.gethostbyname(Unix.gethostname ())).Unix.h_addr_list.(0) in
  let ip = Unix.string_of_inet_addr addr in
  let criteria = sprintf "%s:%d" ip port in
  let criteria = kprintf Str.regexp ".*%s.*" (Str.quote criteria) in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr_inet = Unix.ADDR_INET(addr, port) in
  Unix.bind socket addr_inet;
  Unix.listen socket 3;
  Unix.setsockopt_float socket Unix.SO_RCVTIMEO 5.0;
  Unix.setsockopt_float socket Unix.SO_SNDTIMEO 5.0;
  let active = ref true in
  ignore (Thread.create begin fun () ->
    try
      while !active do
        ignore (Unix.accept socket);
        active := false;
      done;
      Unix.close socket;
    with Unix.Unix_error (e, _, _) -> begin
      Printf.printf "accept: %s\n%!" (Unix.error_message e)
    end;
  end ());
  let cmd = sprintf "netstat -nao" in
  let inchan = Unix.open_process_in cmd in
  set_binary_mode_in inchan false;
  let pid = ref "" in
  begin
    try
      while true do
        let line = input_line inchan in
        if Str.string_match criteria line 0 then begin
          let pos = 1 + String.rindex line ' ' in
          pid := String.sub line pos (String.length line - pos);
          raise End_of_file
        end
      done
    with End_of_file -> ()
  end;
  ignore (Unix.close_process_in inchan);
  printf "%s\n%!" !pid;	
  let len = Array.length Sys.argv - 1 in
  let cmd = Array.create len "" in
  Array.blit Sys.argv 1 cmd 0 len;
  let cmd = Array.to_list cmd in
  let cmd = String.concat " " cmd in
  ignore begin
    Thread.create begin fun () ->
      try
        let inchan, outchan = Unix.open_connection addr_inet in
        Unix.shutdown_connection inchan;
      with Unix.Unix_error (e, _, _) -> begin
        Printf.printf "connection: %s\n%!" (Unix.error_message e)
      end;
    end ()
  end;
  let cmd = sprintf "\"%s\"" cmd in
  exit (Sys.command cmd)


let _ = main()

