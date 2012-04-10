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
  delay          : int; (* ms *)
  mutable buffer : (unit -> unit) option;
  mutable id     : GMain.Timeout.id option;
}

let create ~delay () = {
  delay  = int_of_float (delay *. 1000.);
  buffer = None;
  id     = None;
}

let start liim =
  liim.id <- Some (GMain.Timeout.add ~ms:liim.delay ~callback:begin fun () ->
    Gaux.may liim.buffer ~f:begin fun f ->
      try
        f ();
        liim.buffer <- None;
      with ex -> Printf.eprintf "File \"liim.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
    end;
    true
  end);;

let set liim func = liim.buffer <- Some func

let destroy liim = Gaux.may liim.id ~f:GMain.Timeout.remove
