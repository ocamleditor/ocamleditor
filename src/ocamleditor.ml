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


(** fade_out *)
let fade_out window =
  if Ocaml_config.is_mingw then begin
    GMain.Timeout.add ~ms:5 ~callback:begin fun () ->
      let opa = max 0. (window#opacity -. 0.1) in
      if opa > 0. then begin
        window#set_opacity opa;
        true
      end else begin
        window#destroy();
        false
      end
    end |> ignore;
  end else begin
    while window#opacity > 0. do
      Thread.delay 0.02;
      let opa = max 0. (window#opacity -. 0.1) in
      if opa >= 0. then begin
        window#set_opacity opa;
      end
    done;
  end;
  window#destroy()

(** main *)
let main () = begin
  let _ = About.build_id := Build_id.timestamp in  (*GtkMain.Main.disable_setlocale();*)
  (*Unix.putenv "LANGUAGE" "C";*)
  (*Unix.putenv "GTK_SETLOCALE" "0";*)
  (*let locale = Glib.Main.setlocale `ALL (Some "C") in*)
  let start splashscreen =
    let browser = Browser.create () in
    (* Before browser initialization *)
    browser#connect#startup ~callback:begin fun () ->
      Gaux.may splashscreen ~f:(fun w -> w#set_transient_for browser#window#as_window);
      Sys.chdir (Filename.dirname Sys.executable_name);
      Printf.printf "%s\n%!" (System_properties.to_string());
      Plugin.load "dot_viewer_svg.cma" |> ignore;
      Project_xml.init();
      Gtk_theme.set_theme ~context:browser#window#misc#pango_context ();
      browser#window#misc#connect#show ~callback:begin fun () ->
        Gmisclib.Idle.add ~prio:300 begin fun () ->
          Plugin.load "plugin_diff.cma" |> ignore; (* plugin_diff requires editor pages *)
          Gaux.may splashscreen ~f:fade_out;
          Gaux.may (browser#editor#get_page `ACTIVE) ~f:(fun page -> page#view#misc#grab_focus());
		  ()
        end
      end |> ignore;
    end |> ignore;
    (* After browser initialization (after splashscreen) and before browser window is shown *)
    browser#connect#after#startup ~callback:begin fun () ->
      ()
    end |> ignore;
    (*  *)
    browser#startup();
    browser#window#present();
  in
  begin
    match Browser.splashscreen() with
      | None -> start None
      | Some splashscreen ->
        splashscreen#misc#connect#after#show ~callback:begin fun () ->
          GMain.Timeout.add ~ms:100 ~callback:(fun () -> start (Some splashscreen); false) |> ignore;
          (*Gmisclib.Idle.add ~prio:300 (fun () -> start (Some splashscreen));*)
        end |> ignore;
        splashscreen#present();
  end;
  GtkThread2.main ();
end

let _ =
  Printexc.record_backtrace true;
  try main ()
  with e ->
    Printf.printf "Exception: %s\n" @@ Printexc.to_string e;
    Printexc.print_backtrace stderr




















