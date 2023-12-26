type process = {
  pid : int;
  inchan : in_channel;
  outchan : out_channel;
  errchan : in_channel;
}

type result = [ `PID of int | `ERROR of exn | `SUCCESS ]

(** create_process *)
let create_process ?wd ?env program args =
  let (in_read, in_write) = Unix.pipe() in
  let (out_read, out_write) = Unix.pipe() in
  let (err_read, err_write) = Unix.pipe() in
  let inchan = Unix.in_channel_of_descr in_read in
  let outchan = Unix.out_channel_of_descr out_write in
  let errchan = Unix.in_channel_of_descr err_read in
  let cwd = ref None in
  begin
    match wd with
    | None -> ()
    | Some x ->
        cwd := Some (Sys.getcwd());
        Sys.chdir x
  end;
  let args = Array.of_list (List.filter (fun x -> String.trim x <> "") (Array.to_list args)) in
  (*Printf.printf "SPAWN:\n  %s\n  %s %s\n%!" (Sys.getcwd()) program (String.concat ";" (Array.to_list args));*)
  let args = Array.append [| program |] args in
  try
    let pid =
      match env with
      | None -> Unix.create_process program args out_read in_write err_write
      | Some env -> Unix.create_process_env program args env out_read in_write err_write
    in
    (match !cwd with None -> () | Some x -> Sys.chdir x; cwd := None);
    Unix.close out_read;
    Unix.close in_write;
    Unix.close err_write;
    let proc = { pid; inchan; outchan; errchan } in
    proc
  with
  | Unix.Unix_error (err, a, b) as ex ->
      (match !cwd with None -> () | Some x -> Sys.chdir x; cwd := None);
      (*Printf.eprintf "%s (%S, %S)\n%!" (Unix.error_message err) a b;*)
      raise ex
  | ex ->
      (match !cwd with None -> () | Some x -> Sys.chdir x; cwd := None);
      raise ex


(** loop *)
let loop (f : in_channel -> unit) chan = try while true do f chan done with End_of_file -> ()

(** redirect_to_stdout *)
let redirect_to_stdout = loop (fun chan -> input_line chan |> print_endline)

(** redirect_to_stderr *)
let redirect_to_stderr = loop (fun chan -> input_line chan |> prerr_endline)

(** redirect_to_ignore *)
let redirect_to_ignore = loop (fun chan -> input_line chan |> ignore)

(** exec *)
let exec
    mode
    ?working_directory
    ?env
    ?at_exit
    ?process_in
    ?process_out
    ?process_err
    ?(binary=false)
    program args =
  try
    let proc = create_process ?wd:working_directory ?env program args in
    set_binary_mode_in proc.inchan binary;
    set_binary_mode_in proc.errchan binary;
    set_binary_mode_out proc.outchan binary;
    let process_in = match process_in with Some f -> f | _ -> redirect_to_ignore in
    let process_err = match process_err with Some f -> f | _ -> redirect_to_ignore in
    let final () =
      let f = match at_exit with Some f -> f | _ -> ignore in
      try
        Stdlib.close_in proc.inchan;
        Stdlib.close_in proc.errchan;
        Stdlib.close_out proc.outchan;
        f None
      with (Unix.Unix_error _) as ex -> f (Some ex)
    in
    let tho = match process_out with Some f -> Some (Thread.create f proc.outchan) | _ -> None in
    let thi = Thread.create process_in proc.inchan in
    let the =
      Thread.create begin fun () ->
        process_err proc.errchan;
        Thread.join thi;
        (match tho with Some t -> Thread.join t | _ -> ());
        if mode = `ASYNC then final()
      end ()
    in
    match mode with
    | `SYNC ->
        Thread.join the;
        Thread.join thi;
        (match tho with Some t -> Thread.join t | _ -> ());
        final();
        `SUCCESS
    | `ASYNC -> `PID proc.pid
  with (Unix.Unix_error _) as ex -> `ERROR ex

(** sync *)
let sync
    ?working_directory
    ?env
    ?at_exit
    ?process_in
    ?process_out
    ?process_err
    ?binary
    program args =
  match
    exec `SYNC
      ?working_directory
      ?env
      ?at_exit
      ?process_in
      ?process_out
      ?process_err
      ?binary
      program args
  with
  | `SUCCESS -> None
  | `ERROR ex -> Some ex
  | `PID _ -> assert false

(** async *)
let async
    ?working_directory
    ?env
    ?at_exit
    ?process_in
    ?process_out
    ?process_err
    ?binary
    program args =
  match
    exec `ASYNC
      ?working_directory
      ?env
      ?at_exit
      ?process_in
      ?process_out
      ?process_err
      ?binary
      program args
  with
  | `SUCCESS -> assert false
  | (`ERROR _) as x -> x
  | (`PID _) as x -> x



