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

let confirm ~message ~f widget =
  match GWindow.toplevel widget with
    | None -> ()
    | Some parent ->
      let message = GWindow.message_dialog ~message
        ~message_type:`QUESTION ~position:`CENTER ~allow_grow:false
        ~modal:true ~buttons:GWindow.Buttons.yes_no () in
      message#set_transient_for parent#as_window;
      (match message#run() with
        | `YES -> (fst f) ()
        | `NO -> (snd f) ()
        | _ -> ());
      message#destroy()

let info ?(title="") ~message widget =
  match GWindow.toplevel widget with
    | None -> ()
    | Some parent ->
      if message <> "" then begin
        let message = GWindow.message_dialog ~message ~title
          ~modal:true ~position:`CENTER ~parent ~type_hint:`DIALOG
          ~message_type:`INFO ~buttons:(GWindow.Buttons.ok) () in
        message#set_transient_for parent#as_window;
        ignore(message#run());
        message#destroy()
      end

let display_exn widget e =
  match GWindow.toplevel widget with
    | None -> ()
    | Some parent ->
      let message = GWindow.message_dialog ~message:(Printexc.to_string e)
        ~modal:true ~destroy_with_parent:false ~position:`CENTER ~parent
        ~message_type:`INFO ~buttons:(GWindow.Buttons.ok) () in
      ignore(message#run());
      message#destroy()

let process_still_active ~name ~ok ~cancel () =
  let confirm = GWindow.message_dialog ~title:"Process is still active"
    ~message:(sprintf "Terminate process \"%s\"?" name)
    ~message_type:`QUESTION ~buttons:GWindow.Buttons.ok_cancel ~modal:true ~position:`CENTER () in
  begin match confirm#run () with
    | `OK -> ok()
    | `CANCEL ->
      confirm#destroy();
      cancel()
    | _ -> ()
  end;
  confirm#destroy()








