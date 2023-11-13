open Printf

let create_small_button ?tooltip ?icon ?callback ?packing ?show () =
  let button = GButton.button ~relief:`NONE ?packing ?show () in
  button#set_focus_on_click false;
  button#misc#set_name "smallbutton";
  icon |> Option.iter begin fun icon ->
    let icon = sprintf "<span font='9'>%s</span>" icon in
    (Gtk_util.label_icon (*~height:14*) ~packing:button#add icon)#coerce |> ignore
  end;
  Gaux.may tooltip ~f:button#misc#set_tooltip_text;
  Gaux.may callback ~f:(fun callback -> ignore (button#connect#clicked ~callback));
  button

class gitbar ?packing () =
  let box = GPack.hbox ~spacing:8 ?packing () in
  let create_gitbutton icon =
    let icon = sprintf "<span font='9'>%s</span>" icon in
    let button = new Gtk_util.button_icon (*~icon_width:18*) ~icon_spacing:0 ~icon ~packing:box#add ~relief:`NONE () in
    button#button#set_focus_on_click false;
    button#button#misc#set_name "smallbutton";
    button
  in
  let button_gitunpushed = create_gitbutton "\u{eaa1}" in
  let button_gitpending = create_gitbutton "\u{ea73}" in
  let button_gitpath = create_gitbutton "\u{e65d}" in
  let button_gitbranch = create_gitbutton "\u{f062c}" in
  object (self)
    inherit GObj.widget box#as_widget
    initializer
      button_gitpending#button#connect#clicked ~callback:begin fun () ->
        Git.diff_stat (Git.show_diff_stat None);
      end |> ignore;
    method button_gitunpushed = button_gitunpushed
    method button_gitpending = button_gitpending
    method button_gitpath = button_gitpath
    method button_gitbranch = button_gitbranch
  end

class widget ?(color=false) ?packing () =
  let ebox = GBin.event_box ?packing () in
  let box = GPack.hbox (*~height:21 *)~spacing:1 ~border_width:0 ~packing:ebox#add () in
  let editor_status = GBin.alignment ~packing:(box#pack ~from:`START ~expand:true ~fill:true) () in
  let _ = GMisc.separator `VERTICAL ~packing:(box#pack ~expand:false) () in
  object (self)
    inherit GObj.widget box#as_widget
    initializer
      if color then begin
        self#set_style();
        ebox#misc#toplevel#misc#connect#after#style_set
          ~callback:(fun () -> Gmisclib.Idle.add ~prio:300 self#set_style) |> ignore;
      end

    method set_editor_status bar =
      if editor_status#children <> [] then
        editor_status#remove editor_status#child#coerce;
      editor_status#add bar;

    method pack ?from widget = box#pack ?from ~expand:false ~fill:false widget

    method private set_style () =
      ebox#misc#modify_bg [`NORMAL, `COLOR (ebox#misc#style#dark `SELECTED)];
      ebox#misc#modify_fg [`NORMAL, `COLOR (ebox#misc#style#fg `SELECTED)];
      ebox#misc#modify_text [`NORMAL, `COLOR (ebox#misc#style#text `SELECTED)];
  end

