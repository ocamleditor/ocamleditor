(* $Id: shell.ml,v 1.15 2003/07/06 23:38:55 garrigue Exp $ *)

open StdLabels
module Unix = UnixLabels
open GdkKeysyms
open Printf
open Preferences

(* Nice history class. May reuse *)

class ['a] history () = object
  val mutable history = ([] : 'a list)
  val mutable count = 0
  method empty = history = []
  method add s = count <- 0; history <- s :: history
  method next =
    let s = List.nth history count in
    count <- (count + 1) mod List.length history;
    s
  method previous =
    let l = List.length history in
    count <- (l + count - 1) mod l;
    List.nth history ((l + count - 1) mod l)
end

(* The shell class. Now encapsulated *)

let protect f x = try f x with _ -> ();;

let count = ref 0

class shell ~prog ~(env : string array) ~(args : string list) ?packing ?show () =
  let view = GText.view ~cursor_visible:false ~editable:false ?packing ?show () in
  let buf = view#buffer in
  let process = Process.create ~env ~prog ~args () in
  let _ = Process.start process in
  let inchan, outchan, errchan = Process.channels process in
object (self)
  inherit GObj.widget view#as_widget
  val buffer = buf
  val view = view
  val mutable name = incr count; "Shell "^(string_of_int !count)
  val mutable pid = Process.getpid process
  val mutable h = new history ()
  val mutable alive = true
  val mutable reading = false
  val input_start =
    `MARK (buf#create_mark ~left_gravity:true buf#start_iter)

  method prog = prog
  method name = name
  method set_name n = name <- n
  method private position = buffer#get_iter `INSERT
  method private input_start = buffer#get_iter (input_start :> GText.position)
  method private set_input_start () =
    buffer#move_mark input_start self#position;

  method textview = view
  method alive = alive
  method restart = self#kill

  method quit () =
    kprintf self#insert "#quit;;\n";
    self#return();
    alive <- false;
    self#kill();

  method kill () =
    alive <- false;
    view#misc#set_sensitive false;
    Process.kill process;

  method interrupt () = ()
(*    if alive then try
      reading <- false;
      Unix.kill ~pid ~signal:Sys.sigint
    with Unix.Unix_error _ -> ()*)

  method send s =
    if alive then try
      output_string outchan s;
      flush outchan;
      if s <> "\n" then (h#add s);
    with (Sys_error _) as ex -> (printf "%s\n%!" (Printexc.to_string ex))

  method private read ~fd ~len =
    try
      let buf = String.create len in
      let len = Unix.read ~buf ~pos:0 ~len fd in
      if len > 0 then begin
	buffer#place_cursor buffer#end_iter;
        let txt = String.sub buf ~pos:0 ~len in
	self#insert txt;
	self#set_input_start ();
      end;
      len
    with Unix.Unix_error (e, s1, s2) -> 0

  method history_object = h

  method set_history_object h' = h <- h'

  method delete_input_line () =
    buffer#delete ~start:self#input_start ~stop:self#position;

  method history (dir : [`next|`previous]) =
    if not h#empty then begin
      if reading then begin
        self#delete_input_line();
      end else begin
	reading <- true;
	self#set_input_start ();
      end;
      buffer#place_cursor buffer#end_iter;
      self#insert (if dir = `previous then h#previous else h#next);
      view#misc#grab_focus()
    end

  method private lex ~start ~stop = ()

  method insert text = buffer#insert text

  method private keypress c =
    if not reading && c > " " then begin
      reading <- true;
      self#set_input_start ();
    end

  method (*private*) return () =
    buffer#insert "\n";
    if reading then reading <- false else begin
      let rec search (it : GText.iter) =
        match it#backward_search "# " with None -> it
        | Some (it1, it2) ->
            if it1#starts_line then it2
            else search it1
      in
      buffer#move_mark input_start (search self#position)
    end;
    let stop = self#position#forward_to_line_end in
    buffer#place_cursor stop;
    let s = buffer#get_text ~start:(self#input_start) ~stop () in
    buffer#place_cursor buffer#end_iter;
    self#send s;
    self#send "\n"

  method private paste () =
    if not reading then begin
      reading <- true;
      self#set_input_start ();
    end

  initializer
    buffer#place_cursor buffer#end_iter;
    let txt = "(Press F8 in the editor to evaluate expressions here)\n\n" in
    self#insert txt;
    self#set_input_start ();
    (*  *)
    h#add "";
    view#misc#modify_font_by_name preferences#get.pref_base_font;
    view#misc#set_size_chars ~width:80 ~height:25 ~lang:"C" ();
    view#event#connect#key_press ~callback:
      begin fun ev ->
        let key = GdkEvent.Key.keyval ev in
        let state = GdkEvent.Key.state ev in
	(*if key = _Return && state = [] then (self#return (); true)
        else *)if key = _Up && state = [`CONTROL] then (self#history `next; true)
        else if key = _Down && state = [`CONTROL] then (self#history `previous; true)
	else true(*(self#keypress (GdkEvent.Key.string ev); false)*);
      end;
    buffer#connect#after#insert_text ~callback:
      begin fun it s ->
        try
          let start = it#backward_chars (String.length s) in
          self#lex ~start:(start#set_line_index 0) ~stop:it#forward_to_line_end;
          view#scroll_mark_onscreen `INSERT;
        with Gpointer.Null -> ()
      end;
    buffer#connect#after#delete_range ~callback:
      begin fun ~start ~stop ->
        let start = start#set_line_index 0
        and stop = start#forward_to_line_end in
        self#lex ~start ~stop
      end;
    view#event#connect#button_press ~callback:
      begin fun ev ->
	if GdkEvent.Button.button ev = 2 then self#paste ();
	false
      end;
(*    view#event#connect#button_press ~callback:(fun _ -> true);*)
    view#connect#destroy ~callback:self#kill;

    ignore (List.map begin fun fd ->
      Thread.create begin fun () ->
        while alive do
          self#read ~fd ~len:1024
        done
      end ();
    end (List.map Unix.descr_of_in_channel [errchan; inchan]));

end




class ocaml ~prog ~env ~args ?packing ?show () =
object (self)
  inherit shell ~prog ~env ~args ?packing ?show ()
  method private lex ~start ~stop =
    if start#compare stop < 0 then Lexical.tag buffer ~start ~stop
  initializer
    Lexical.init_tags buffer;
end


