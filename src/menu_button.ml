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

let icon_normal =
  GdkPixbuf.from_xpm_data [|
    "5 16 2 1";
    " 	c None";
    ".	c #000000";
    "     ";
    "     ";
    "     ";
    "     ";
    "     ";
    "     ";
    "     ";
    ".....";
    " ... ";
    "  .  ";
    "     ";
    "     ";
    "     ";
    "     ";
    "     ";
    "     "|];;

let icon_pressed =
  GdkPixbuf.from_xpm_data [|
    "5 16 2 1";
    " 	c None";
    ".	c #000000";
    "     ";
    "     ";
    "     ";
    "     ";
    "     ";
    "     ";
    "     ";
    "     ";
    ".....";
    " ... ";
    "  .  ";
    "     ";
    "     ";
    "     ";
    "     ";
    "     "|];;


class menu_button ?(label="") ?(relief=`NORMAL) ?stock ?packing () =
  let box = GPack.hbox ~spacing:0 ?packing () in
  let button = GButton.button ~relief ?stock ~packing:box#pack () in
  let button_menu = GButton.button ~relief ~packing:box#pack () in
  let clicked = new clicked () in
  let popup = new popup () in
object (self)
  inherit GObj.widget box#as_widget
  val relief = relief
  val mutable gmenu = None
  val image_pressed = (GMisc.image ~pixbuf:icon_pressed ())#coerce
  val image_normal = (GMisc.image ~pixbuf:icon_normal ())#coerce
  val mutable tooltip_text = None

  initializer
    ignore (button#connect#clicked ~callback:clicked#call);
    button_menu#set_image image_normal;
    button#set_focus_on_click false;
    button_menu#set_focus_on_click false;
    ignore (button#connect#enter ~callback:begin fun ev ->
      button_menu#misc#set_state `PRELIGHT;
    end);
    ignore (button#connect#leave ~callback:begin fun ev ->
      button_menu#set_image image_normal;
      button_menu#misc#set_state `NORMAL;
      button_menu#set_relief relief;
    end);
    ignore (button#connect#pressed ~callback:begin fun ev ->
      button_menu#set_image image_pressed;
      button_menu#misc#set_state `ACTIVE;
      button_menu#set_relief `HALF;
    end);
    ignore (button#connect#released ~callback:begin fun ev ->
      button_menu#set_image image_normal;
      button_menu#misc#set_state `NORMAL;
    end);
    ignore (button_menu#connect#enter ~callback:begin fun ev ->
      button#misc#set_state `PRELIGHT;
    end);
    ignore (button_menu#connect#leave ~callback:begin fun ev ->
      button#misc#set_state `NORMAL;
    end);
    ignore (button_menu#event#connect#button_press ~callback:begin fun ev ->
      tooltip_text <- Some (GtkBase.Widget.Tooltip.get_text box#as_widget);
      box#misc#set_tooltip_text "";
      button_menu#set_image image_pressed;
      let time = GdkEvent.Button.time ev in
      let pos ~x ~y ~pushed_in =
        let x0, y0 = Gdk.Window.get_position button#misc#window in
        x0 + button#misc#allocation.Gtk.x,
        (y0 + button#misc#allocation.Gtk.y + button#misc#allocation.Gtk.height),
        true
      in
      let menu = GMenu.menu () in
      gmenu <- Some menu;
      let rlabel = ref (Some label) in
      popup#call (rlabel, menu);
      begin
        match !rlabel with
          | Some default_label ->
            if List.length menu#children > 0 then (ignore (GMenu.separator_item ~packing:menu#prepend ()));
            let default_item = GMenu.menu_item ~packing:menu#prepend () in
            default_item#add (GMisc.label ~markup:("<b>"^default_label^"</b>") ~xalign:0.0 ())#coerce;
            ignore (default_item#connect#activate ~callback:button#clicked);
          | None -> ()
      end;
      ignore (menu#connect#deactivate ~callback:begin fun () ->
        button_menu#set_image image_normal;
        button_menu#misc#set_state `NORMAL;
        button_menu#set_relief relief;
        button#misc#set_state `NORMAL;
        button#set_relief relief;
        Gaux.may tooltip_text ~f:box#misc#set_tooltip_text;
        tooltip_text <- None;
      end);
      GtkMenu.Menu.popup_at menu#as_menu ~button:(GdkEvent.Button.button ev) ~time pos;
      button_menu#misc#set_state `ACTIVE;
      button_menu#set_relief `NORMAL;
      button#misc#set_state `PRELIGHT;
      button#set_relief `NORMAL;
      true
    end);
    ignore (button_menu#connect#released ~callback:begin fun () ->
      Gaux.may tooltip_text ~f:box#misc#set_tooltip_text;
      tooltip_text <- None;
      Gaux.may gmenu ~f:begin fun m ->
        ignore (m#popdown());
        gmenu <- None
      end;
    end);

  method set_image = button#set_image
  method connect = new signals ~clicked ~popup
end

and clicked () = object inherit [unit] GUtil.signal () end
and popup () = object inherit [string option ref * GMenu.menu] GUtil.signal () end

and signals ~clicked ~popup =
object
  inherit GUtil.ml_signals [clicked#disconnect; popup#disconnect]
  method clicked = clicked#connect ~after
  method popup = popup#connect ~after
end

let create = new menu_button










