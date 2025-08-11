module type LOCK_OPS = sig
  type name
  val use : name -> (unit -> 'a) -> 'a
end

let create (type a) (names : a list) =
  let locks = names |> List.map (fun name -> name, Mutex.create ()) in
  (module struct
    type name = a
    let use name = Mutex.protect (List.assoc name locks)
  end : LOCK_OPS with type name = a)

(*type lock_name = A | B | Database

  let _ =
  let module L = (val create [A; B; Database]) in
  let x = L.sync A (fun () -> 42) in
  let y = L.sync Database (fun () -> "data") in
  ()
*)
