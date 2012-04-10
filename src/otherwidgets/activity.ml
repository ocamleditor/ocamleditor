(*

  OCamlEditor
  Copyright (C) 2010-2012 Francesco Tovagliari

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


type kind = Annot | Symbol | Outline | Task | Compile_buffer

and t = kind * string (* message/id *)

let table = new GUtil.variable ([] : t list)

let add kind msg = table#set ((kind, msg) :: table#get)

let remove msg = table#set (List.filter (fun (_, m) -> m <> msg) table#get)

(** Activity Monitor *)
let monitor ~message ~monitor ~f () =
  let window    = GWindow.window
    ~icon:Icons.oe
    ~title:"Activity Monitor"
    ~modal:true
    ~type_hint:`UTILITY
    ~position:`CENTER
    ~show:false ()
  in
  window#set_skip_pager_hint true;
  window#set_skip_taskbar_hint true;
  window#set_resizable false;
  let vbox      = GPack.vbox ~spacing:8 ~border_width:8 ~packing:window#add () in
  let hbox      = GPack.hbox ~spacing:8 ~packing:vbox#pack () in
  let spinner   = GMisc.image ~file:(Filename.concat Common.application_pixmaps "spinner.gif") ~packing:hbox#pack () in
  let label     = GMisc.label ~text:message ~width:250 ~xalign:0.0 ~yalign:0.5 ~packing:hbox#pack () in
  let button_ok = GButton.button ~label:"Background" ~packing:hbox#pack () in
  let id        = GMain.Timeout.add ~ms:100 ~callback:begin fun () ->
    let msg = monitor () in
    begin
      match msg with
        | None ->
          ignore(f());
          window#destroy();
        | Some msg -> label#set_text msg
    end;
    msg <> None
  end in
  ignore (button_ok#connect#clicked ~callback:begin fun () ->
    GMain.Timeout.remove id;
    window#destroy();
  end);
  window#show()
;;

(** wrap *)
let wrap kind f x =
  let is_active () = try Some (List.assoc kind table#get) with Not_found -> None in
  if is_active () <> None then begin
    let message = List.assoc kind table#get in
    monitor ~message ~monitor:is_active ~f ();
  end else (f())









