type 'a t = {
  mutex : Mutex.t;
  notempty : Condition.t;
  mutable value : 'a option;
}

let create initial_value = {
  mutex = Mutex.create ();
  notempty = Condition.create();
  value = initial_value
}

let set ev value =
  Mutex.lock ev.mutex;
  ev.value <- Some value;
  Condition.signal ev.notempty;
  Mutex.unlock ev.mutex

let reset ev =
  Mutex.lock ev.mutex;
  ev.value <- None;
  Mutex.unlock ev.mutex

let wait ev =
  Mutex.lock ev.mutex;
  while ev.value = None do
    Condition.wait ev.notempty ev.mutex
  done;
  Mutex.unlock ev.mutex;
  match ev.value with
  | Some v -> v
  | _ -> failwith "wait no value"
