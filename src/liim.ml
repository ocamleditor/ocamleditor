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

type t = {
  delay  : int; (* ms *)
  prio   : int; (* unused *)
  mutable buffer : (unit -> unit) option;
}

let create ~delay ?(prio=300) () = {
  delay  = int_of_float (delay *. 1000.);
  prio   = prio;
  buffer = None;
}

let signal lac func = lac.buffer <- Some func

let start lac =
  GMain.Timeout.add ~ms:lac.delay ~callback:begin fun () ->
    Gaux.may lac.buffer ~f:begin fun f ->
      try
        f ();
        lac.buffer <- None;
      with ex -> Printf.eprintf "File \"liim.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
    end;
    true
  end;;
