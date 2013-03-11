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

(** window *)
let rename ~editor ~page () =
  match page#file with
    | None -> ()
    | Some file ->
      let filename =
        match file#remote with
          | None -> file#filename
          | Some rmt -> sprintf "%s@%s:%s" rmt.Editor_file_type.user rmt.Editor_file_type.host file#filename
      in
      let window = GWindow.dialog
          ~icon:Icons.oe ~title:(sprintf "Rename file?")
          ~position:`CENTER ~modal:true ~show:false ()
      in
      let _ = GMisc.label ~text:(sprintf "Rename remote file\n\n%s" filename) ~xalign:0.0 ~packing:window#vbox#pack () in
      let entry = GEdit.entry ~text:page#get_filename ~packing:window#vbox#pack () in
      window#add_button_stock `OK `OK;
      window#add_button_stock `CANCEL `CANCEL;
      window#set_border_width 8;
      window#vbox#set_spacing 5;
      ignore (window#event#connect#key_release ~callback:begin fun ev ->
          let key = GdkEvent.Key.keyval ev in
          if key = GdkKeysyms._Escape then (window#response `CANCEL; true)
          else if key = GdkKeysyms._Return then begin
            window#response `OK;
            true
          end else false
        end);
      let rec run () =
        match window#run () with
          | `OK ->
            let filename = entry#text in
            if filename <> page#get_filename then begin
              let new_filename_exists =
                let remote =
                  match page#file with
                    | Some file -> (match file#remote with Some remote -> remote | _ -> assert false)
                    | _ -> assert false
                in
                let newfile = Editor_file.create ~remote filename in
                newfile#exists
              in
              if new_filename_exists then begin
                let overwrite () =
                  List_opt.may_find (fun p -> p#get_filename = filename) editor#pages editor#close ();
                  Dialog_rename.rename ~editor ~page ~filename ();
                in
                Dialog_rename.ask_overwrite ~run ~overwrite ~filename window
              end else begin
                Dialog_rename.rename ~editor ~page ~filename ();
                window#destroy()
              end
            end else window#destroy()
          | _ -> window#destroy()
      in run()

(** save_as *)
let save_as ~editor ~page () =
  match page#file with
    | None -> ()
    | Some file ->
      let filename =
        match file#remote with
          | None -> file#filename
          | Some rmt -> sprintf "%s@%s:%s" rmt.Editor_file_type.user rmt.Editor_file_type.host file#filename
      in
      let window = GWindow.dialog
          ~icon:Icons.oe ~title:"Save as..."
          ~position:`CENTER ~modal:true ~show:false ()
      in
      let _ = GMisc.label ~text:(sprintf "Save remote file\n\n%s\n\nas..." filename) ~xalign:0.0 ~packing:window#vbox#pack () in
      let entry = GEdit.entry ~text:page#get_filename ~packing:window#vbox#pack () in
      window#add_button_stock `OK `OK;
      window#add_button_stock `CANCEL `CANCEL;
      window#set_border_width 8;
      window#vbox#set_spacing 5;
      ignore (window#event#connect#key_release ~callback:begin fun ev ->
          let key = GdkEvent.Key.keyval ev in
          if key = GdkKeysyms._Escape then (window#response `CANCEL; true)
          else if key = GdkKeysyms._Return then begin
            window#response `OK;
            true
          end else false
        end);
      let rec run () =
        match window#run () with
          | `OK ->
            let filename = entry#text in
            if filename <> page#get_filename then begin
              let new_filename_exists =
                let remote =
                  match page#file with
                    | Some file -> (match file#remote with Some remote -> remote | _ -> assert false)
                    | _ -> assert false
                in
                let newfile = Editor_file.create ~remote filename in
                newfile#exists
              in
              if new_filename_exists then begin
                let overwrite () =
                  ()
                in
                Dialog_rename.ask_overwrite ~run ~overwrite ~filename window
              end else begin
                (*save_as();*)
                window#destroy()
              end
            end else window#destroy()
          | _ -> window#destroy()
      in run()
