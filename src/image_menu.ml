
let item ~label ?(pixbuf=Icons.empty_8) ?stock ?(icon_size=`MENU) ?(show=true) ?packing () =
  let menu_item = GMenu.menu_item ?packing ~show () in
  let hbox = GPack.hbox ~border_width:2 ~spacing:6 ~packing: menu_item#add () in
  hbox#set_halign `START;
    if Option.is_none stock then
      GMisc.image ~pixbuf ~icon_size ~packing: hbox#add ()
    else
      GMisc.image ?stock ~packing: hbox#add ()
  ;
  GMisc.label ~text: label ~packing: hbox#add ();
  menu_item
;;
