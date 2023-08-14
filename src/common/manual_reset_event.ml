type t = {
  mutex : Mutex.t;
  cond : Condition.t;
  mutable value : bool
}

let create initial_value =
  { mutex = Mutex.create (); cond = Condition.create(); value = initial_value }

let set ev =
  Mutex.lock ev.mutex;
  ev.value <- true;
  Condition.signal ev.cond;
  Mutex.unlock ev.mutex

let reset ev =
  Mutex.lock ev.mutex;
  ev.value <- false;
  Mutex.unlock ev.mutex

let wait ev =
  Mutex.lock ev.mutex;
  while not ev.value do
    Condition.wait ev.cond ev.mutex
  done;
  Mutex.unlock ev.mutex
