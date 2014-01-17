(*let _ =
  match !Browser.browser with
    | Some browser ->
      let menu = browser#menu in
      let _, file_menu = List.assoc 4 menu in
      let item = GMenu.image_menu_item ~label:"Test" ~packing:file_menu#prepend () in
      ()
    | _ -> ()*)

  