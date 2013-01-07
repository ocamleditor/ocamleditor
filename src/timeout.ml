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

type t = {
  delay          : int; (* ms *)
  mutable buffer : (unit -> unit) option array;
  mutable id     : GMain.Timeout.id option;
}

let create ~delay ?(len=1) () = {
  delay  = int_of_float (delay *. 1000.);
  buffer = Array.make len None;
  id     = None;
}

let start timeout =
  timeout.id <- Some (GMain.Timeout.add ~ms:timeout.delay ~callback:begin fun () ->
    Array.iteri begin fun i -> function
      | Some f ->
      (*try*)
        f ();
        Array.unsafe_set timeout.buffer i None;
      (*with ex -> Printf.eprintf "File \"timeout.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());*)
      | _ -> ()
    end timeout.buffer;
    true
  end);;

let set timeout i func = Array.unsafe_set timeout.buffer i (Some func)

let destroy timeout = Gaux.may timeout.id ~f:GMain.Timeout.remove
