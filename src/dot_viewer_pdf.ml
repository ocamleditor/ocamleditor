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

(*class virtual viewer =
object
  method virtual display : filename:string -> unit
  method virtual coerce : GObj.widget
  method virtual destroy : unit -> unit
end
*)

module PDF = struct

  let create ?packing () = None

  let viewer_cmd ~filename =
    let cmd = if Sys.os_type = "Win32" then "" else Preferences.preferences#get.Preferences.pref_pdf_viewer in
    sprintf "%s %s" cmd filename

  let open_file filename =
    ignore (Thread.create begin fun () ->
      begin
        try
          let cmd =
            let filename = Filename.quote filename in
            viewer_cmd ~filename
          in
          ignore (Sys.command cmd)
        with ex -> Printf.printf "Dot.open_file: %s\n%!" (Printexc.to_string ex)
      end;
      Thread.delay 3.0;
      let rec try_remove n =
        try Sys.remove filename with _ ->
          if n > 0 then begin
            Thread.delay 1.;
            try_remove (n-1)
          end
      in
      try_remove 30
    end ());;

  let lang = "pdf"

  let have_embedded_viewer = false

  let draw ~filename _ = open_file filename

end


(*module type DEVICE = sig
  val lang : string
  val have_embedded_viewer : bool
  val create : ?packing:(GObj.widget -> unit) -> unit -> viewer option
  val draw : filename:string -> viewer option -> unit
end


let device : (module DEVICE) ref = ref (module PDF : DEVICE)
*)
