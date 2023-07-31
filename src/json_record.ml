open Printf

let name = "[a-z][a-zA-Z0-9_']*";;
let ws = "[ \t\r\n]";;
let re_semicolon = kprintf Str.regexp "%s*;%s*" ws ws;;
let re_colon = kprintf Str.regexp "%s*:%s*" ws ws;;
let fields = sprintf "%s*\\([a-z][a-zA-Z0-9_':;()* \t\r\n]+\\)%s*" ws ws;;
let re = kprintf Str.regexp "\\(\\(type\\)\\|\\(and\\)\\)%s+\\(%s\\)%s*=%s*{%s}%s*" ws name ws ws fields ws;;

let generate_deserializer is_toplevel name defs =
  sprintf {|
let deserialize_%s json =

|} name
;;
let zzz_create ~(buffer : GText.buffer) ~start ~stop =
  try
    let text = buffer#get_text ~start ~stop () |> String.trim in
    if Str.string_match re text 0 then begin
      let is_toplevel = Str.matched_group 1 text = "type" in
      let type_name = Str.matched_group 4 text in
      let type_desc = Str.matched_group 5 text in
      let defs =
        Str.split re_semicolon type_desc
        |> List.map (Str.split re_colon)
        |> List.map (function [n; t] -> n, t | _ -> "ERROR", "ERROR" )
      in
      defs |> List.iter (fun (n, t) -> Printf.printf "  %s : %s\n%!" n t);
      let deserializer_code = generate_deserializer is_toplevel type_name defs in
      deserializer_code |> printf "%s\n%!"
    end else
      Printf.printf "no match\n%!"
  with ex -> Printf.eprintf "File \"json_record.ml\": %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace());
;;

(*let s = "type foo = { f1 : string; f2 : int }";;
  Str.string_match re s 0;;
  Str.matched_group 0 s;;
  Str.matched_group 1 s;;
  Str.matched_group 2 s;;
  Str.matched_group 3 s;;
  Str.matched_group 4 s;;
  Str.matched_group 5 s;;
*)

