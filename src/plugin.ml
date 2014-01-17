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

open Miscellanea

let loaded = ref []

(** load *)
let load basename =
  let load filename =
    let filename = Dynlink.adapt_filename filename in
    Printf.printf "Plugin: %s... (file_exists=%b) %!" filename (Sys.file_exists filename);
    if not (List.mem filename !loaded) && Sys.file_exists filename then begin
      try
        Dynlink.allow_unsafe_modules true;
        Dynlink.loadfile filename;
        loaded := filename :: !loaded;
        Printf.printf "Loaded\n%!";
        true
      with Dynlink.Error error -> begin
        Printf.printf "Error loading plugin: %s\n%!" (Dynlink.error_message error);
        (*        Dialog.message
                  ~title:"Error while loading plugin"
                  ~message:(sprintf "%s" (Dynlink.error_message error))
                  `ERROR*)
        false
        end
    end else begin
      Printf.printf "Not loaded\n%!" ;
      false
    end
  in
  load (App_config.application_plugins // basename)
