open Printf

module Log = Common.Log.Make(struct let prefix = "OCP-INDEX" end)
let _ =
  Log.set_print_timestamp true;
  Log.set_verbosity `ERROR

let (//) = Filename.concat
let [@inline] (!!) none some = Option.fold ~none ~some
let (~~) opt f = f opt

let loop (f : in_channel -> unit) ((ic, _oc, _ec) as channels) =
  try while true do f ic done
  with End_of_file ->
    (try Unix.close_process_full channels |> ignore
     with Unix.Unix_error(Unix.ECHILD, _ , _) -> ());

type context = string * int option * int option

let execute command ?(context:context option) ~full_open ident =
  let context =
    context |> !! "" (fun (fn, line, col) ->
        line |> !! (sprintf "%s" fn) (fun ln ->
            col |> !! (sprintf "%s:%d" fn ln) (sprintf "%s:%d,%d" fn ln)))
  in
  let context = if context = "" then "" else "--context=" ^ context in
  let full_open = match full_open with [] -> "" | _ -> String.concat "," full_open |> sprintf "-F %s" in
  let build = "--build=." in
  let i_dirs = "-I ." in
  let arguments = [| command; context; full_open; build; i_dirs; ident |] in
  let cmd_line = String.concat " " ("ocp-index":: (arguments |> Array.to_list)) in
  let channels = Unix.open_process_full cmd_line (Unix.environment ()) in
  Log.println `DEBUG "%s" cmd_line;
  let pid = Unix.process_full_pid channels in
  pid, channels

let fullname ~context ident =
  let fst_line = ref None in
  let pid, channels = execute "print" ~context ~full_open:[] ident in
  let parse_line ic =
    let line = input_line ic in
    match !fst_line with
    | None -> fst_line := Some (String.trim line)
    | _ -> ()
  in
  Thread.create (loop parse_line) channels |> ignore;
  (try Unix.waitpid [] pid |> ignore with Unix.Unix_error(Unix.ECHILD, _, _) -> ());
  Option.map (fun x -> String.sub x 0 (String.index x ' ')) !fst_line


