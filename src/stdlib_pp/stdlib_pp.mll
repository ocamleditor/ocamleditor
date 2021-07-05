{}

rule add_stdlib_prefix = parse
  | "module" [' ' '\t']+ (['A'-'Z' 'a'-'z' '0'-'9']+ as mod_name) ' '+ "=" ' '+  ['A'-'Z' 'a'-'z' '0'-'9']+ 
  { print_string @@ "module " ^ mod_name ^ " = Stdlib__" ^ (String.uncapitalize_ascii mod_name) }
  | _ as c
  { print_char c }
  | eof
  { exit 0 }

{
let main () =
  let ch =
    if Array.length Sys.argv > 1 then
      let filename = Sys.argv.(1) in
      let _ = print_endline @@ "# 1 \"" ^ filename ^ "\"" in
      open_in filename 
    else 
      stdin 
  in
  let lexbuf = Lexing.from_channel ch in
  while true do
    add_stdlib_prefix lexbuf
  done

let _ = Printexc.print main ()
}
