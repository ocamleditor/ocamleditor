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

open Printf

let is_debug = false && App_config.application_debug
let host = if is_debug then "ocamleditor.forge.ocamlcore.org" else "raw.github.com"
(*"git.ocamlcore.org"*)
let path = if is_debug then "/VERSION_TEST.txt" else "/ftovagliari/ocamleditor/master/VERSION"
(*"/cgi-bin/gitweb.cgi?p=ocamleditor/ocamleditor.git;a=blob_plain;f=VERSION;hb=HEAD"*)
let re = Str.regexp "^\\([0-9]+\\.[0-9]+\\(\\.[0-9]+\\)?\\)\r?$"

(** init_socket *)
let init_socket addr port =
  let inet_addr = (Unix.gethostbyname addr).Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  let suck = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect suck sockaddr;
  let outchan = Unix.out_channel_of_descr suck in
  let inchan = Unix.in_channel_of_descr suck in
  (suck, inchan, outchan)

(** submit_request *)
let submit_request ?(close=false) req_string path outchan =
  kprintf (output_string outchan) "%s %s HTTP/1.1\r\n" req_string path;
  kprintf (output_string outchan) "Host: %s:80\r\n" host;
  if close then (kprintf (output_string outchan) "Connection: close\r\n");
  kprintf (output_string outchan) "\r\n";
  flush outchan

(** parse *)
let parse current_version result inchan =
  try
    while true do
      let line = input_line inchan in
      if Str.string_match re line 0 then begin
        let version = Str.matched_group 1 line in
        if version > current_version then (result := Some (Str.matched_group 1 line));
        raise End_of_file
      end
    done;
  with End_of_file -> (flush stdout)

(** check *)
let check current_version () = begin
  let result = ref None in
  begin
    try
      let address, port = host, 80 in
      let (sock, inchan, outchan) = init_socket address port in
      ignore (submit_request ~close:true "GET" path outchan);
      ignore (parse current_version result inchan);
      Unix.shutdown sock Unix.SHUTDOWN_ALL;
    with (Sys_error _) as ex -> begin
      Printf.fprintf stderr "%s\n%!" (Printexc.to_string ex)
    end
  end;
  !result
end

