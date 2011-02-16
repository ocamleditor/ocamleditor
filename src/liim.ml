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
  delay : float;
  prio : int;
  mutex : Mutex.t;
  cond : Condition.t;
  mutable buffer : (unit -> unit) option;
}

let create ~delay ?(prio=300) () = {
  delay = delay;
  prio = prio;
  mutex = Mutex.create ();
  cond = Condition.create ();
  buffer = None;
}

let signal lac func =
  Mutex.lock lac.mutex;
  lac.buffer <- Some func;
  Condition.signal lac.cond;
  Mutex.unlock lac.mutex

let start_thread lac =
  Thread.create begin fun () -> 
    while true do
      Mutex.lock lac.mutex;
      Condition.wait lac.cond lac.mutex;
      Mutex.unlock lac.mutex;
      Thread.delay lac.delay;
      Mutex.lock lac.mutex;
      begin
        try
          Gaux.may lac.buffer ~f:begin fun act ->
            Gtk_util.idle_add ~prio:lac.prio begin fun () ->
              try GtkThread2.async act ()
              with
                | Sys_error("_none_: No such file or directory") -> ()
                | ex -> (eprintf "Liim thread (idle): %s\n%!" (Printexc.to_string ex))
            end;
            lac.buffer <- None;
          end;
        with ex -> (eprintf "%s\n%!" (Printexc.to_string ex))
      end;
      Mutex.unlock lac.mutex;
    done
  end ()
