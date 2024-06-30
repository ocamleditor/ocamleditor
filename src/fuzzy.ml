open Printf;;

let distance s t =
  let m = String.length s in
  let n = String.length t in
  let d = Array.make_matrix (m + 1) (n + 1) 0 in
  for i = 1 to m do d.(i).(0) <- i done;
  for j = 1 to n do d.(0).(j) <- j done;
  for j = 1 to n do
    for i = 1 to m do
      d.(i).(j) <-
        if s.[i - 1] = t.[j - 1] then d.(i - 1).(j - 1)
        else
          let deletion = d.(i - 1).(j) + 1 in
          let insertion = d.(i).(j - 1) + 1 in
          let substitution = d.(i - 1).(j - 1) + 1 in
          min (min deletion insertion) substitution
    done
  done;
  d.(m).(n);;

let lcs =
  let longest xs ys = if List.length xs > List.length ys then xs else ys in
  let list_of_string str =
    let result = ref [] in
    String.iter (fun x -> result := x :: !result)
      str;
    List.rev !result
  in
  fun xs' ys' ->
    let xs' = list_of_string xs' in
    let ys' = list_of_string ys' in
    let xs = Array.of_list xs'
    and ys = Array.of_list ys' in
    let n = Array.length xs
    and m = Array.length ys in
    let a = Array.make_matrix (n+1) (m+1) [] in
    for i = n-1 downto 0 do
      for j = m-1 downto 0 do
        a.(i).(j) <- if xs.(i) = ys.(j) then
            xs.(i) :: a.(i+1).(j+1)
          else
            longest a.(i).(j+1) a.(i+1).(j)
      done
    done;
    a.(0).(0);;

let chars_of_string s = s |> String.lowercase_ascii |> String.to_seq |> Array.of_seq;;

let print_matrix m =
  Printf.printf "\n%!";
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length (m.(i)) - 1 do
      if m.(i).(j) <> '\x00' then Printf.printf "%c" m.(i).(j) else Printf.printf " "
    done;
    Printf.printf "\n%!"
  done;;

let print_path_matrix la lb paths =
  let m = Array.make_matrix la lb '\x00' in
  paths |> Array.iteri (fun i n ->
      if n > 0 then begin
        let a = i / lb in
        let b = i mod lb in
        m.(a).(b) <- '*';
        let a = n / lb in
        let b = n mod lb in
        m.(a).(b) <- '*'
      end);
  print_matrix m;;

let matching_positions a b =
  let a = chars_of_string a in
  let b = chars_of_string b in
  let la = Array.length a in
  let lb = Array.length b in
  let m = Array.make_matrix la lb '\x00' in
  let count = ref 0 in
  let ind = ref [] in
  for i = 0 to la - 1 do
    for j = 0 to lb - 1 do
      if Array.unsafe_get a i = Array.unsafe_get b j then begin
        m.(i).(j) <- Array.unsafe_get a i;
        ind := (i, j) :: !ind;
        incr count
      end
    done;
  done;
  (*print_matrix m;*)
  la, lb, m, !ind, !count;;

let matching_paths la lb m ind =
  let mapping = Array.make (la * lb) 0 in
  let neighbors (i, j) =
    if i > 0 && j > 0 && m.(i - 1).(j - 1) <> '\x00' then
      let start = (i - 1) * lb + (j - 1) in
      let stop = i * lb + j in
      (*Printf.printf "%d,%d -> %d,%d  --  %d -> %d\n%!" (i-1) (j-1) i j start stop;*)
      mapping.(start) <- stop;
  in
  ind |> List.iter neighbors;
  mapping;;

let join edges =
  let paths = ref [] in
  let rec follow i =
    let j = edges.(i) in
    edges.(i) <- 0;
    if j = 0 then [i] else i :: follow j
  in
  for i = 0 to Array.length edges - 1 do
    if edges.(i) > 0 then paths := (follow i) :: !paths;
  done;
  !paths;;

let remove_shortest_overlapping lb paths =
  let paths = Array.of_list paths in
  let len = Array.length paths in
  for i = 0 to len - 1 do
    if paths.(i) <> [] then begin
      let rooti = List.hd paths.(i) in
      let bi = rooti / lb in
      for j = i + 1 to len - 1 do
        if paths.(j) <> [] then begin
          let ij_overlap = paths.(j) |> List.exists (fun n -> n / lb = bi) in
          if ij_overlap then begin
            if List.length paths.(i) <= List.length paths.(j) then
              paths.(i) <- []
            else
              paths.(j) <- []
          end
        end
      done
    end
  done;
  paths |> Array.to_list |> List.filter (fun p -> p <> []);;

let compare pat =
  let lpat = float (String.length pat) in
  fun str ->
    let lstr = float (String.length str) in
    let la, lb, m, ind, count = matching_positions pat str in
    if count > 0 then
      begin
        let paths = matching_paths la lb m ind in
        let paths = paths |> join |> remove_shortest_overlapping lb in
        let amount = List.fold_left (fun sum l -> List.length l + sum) 0 paths |> float in
        let compactness = amount /. float (List.length paths) in
        let s_relevance = amount /. lstr in
        let p_relevance = amount /. lpat in
        let top = lpat +. lpat +. 2. in
        let score = amount +. compactness +. s_relevance +. p_relevance in
        let score_perc = score /. top in
        (*pat, str, score_perc, (amount, compactness, s_relevance, p_relevance);*)
        if score_perc >= 0.62 then score_perc else 0.
      end else 0.;
;;
