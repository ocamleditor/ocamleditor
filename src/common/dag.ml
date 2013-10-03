open Printf

type 'a t = 'a entry list

and 'a entry = {
  node                 : 'a;
  mutable dependencies : 'a entry list;
  mutable dependants   : 'a entry list;
}

let set_dependants (dag : 'a t) =
  List.iter begin fun entry ->
    List.iter begin fun node ->
      node.dependants <- entry :: node.dependants
    end entry.dependencies
  end dag

let create filename =
  let dag' = Dep_ext.find' filename in
  let dag =
    Dep_ext.fold begin fun node deps acc ->
      { node; dependencies = []; dependants = [] } :: acc
    end dag' []
  in
  Dep_ext.iter begin fun node deps ->
    try
      let node = List.find (fun e -> e.node = node) dag in
      List.iter begin fun dep ->
        let e = List.find (fun e -> e.node = dep) dag in
        node.dependencies <- e :: node.dependencies;
      end deps;
    with Not_found -> assert false
  end dag';
  set_dependants dag;
  dag

let find_leaves dag =
  List.fold_left begin fun acc node ->
    if node.dependencies = [] then node :: acc else acc
  end [] dag

let test = create "editor_page.ml";;

let leaves = find_leaves test;;

List.map (fun entry -> entry.node) leaves

(*

#load "C:\\ocaml\\lib\\str.cma";;
#load "C:\\ocaml\\lib\\unix.cma";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\cmd.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\app_config.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\ocaml_config.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\miscellanea.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\file_util.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\dep.cmo";;
#load "C:\\ocaml\\devel\\ocamleditor\\src\\common\\dep_ext.cmo";;
#directory "C:\\ocaml\\devel\\ocamleditor\\src\\common"




let dag = find' "editor_page.ml" in File_util.write "D://temp/test.dot" (dot_of_dag dag);;

let dag = find' "editor_page.ml" in for i = 1 to 5 do analyze dag |> ignore done; File_util.write "D://temp/test.dot" (dot_of_dag dag);;

let dag = find' "prj.ml" in analyze dag;;

let dag = find' "editor_page.ml" in for i = 1 to 3 do
  let leaves = analyze dag in
   List.iter begin fun g ->
    Printf.printf "%s\n%!" (String.concat ", " g);
    Printf.printf "-----------------------\n%!" ;
  end leaves;
  Printf.printf "************************\n%!" ;
done;;

*)