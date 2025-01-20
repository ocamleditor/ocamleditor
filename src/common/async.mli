type 'a expr
type 'a task
(*val return : ?name:string -> (unit -> 'a) -> 'a expr*)
val create : ?name:string -> (unit -> 'a) -> 'a expr
(*val bind : ('a -> 'b expr) -> 'a expr -> 'b expr
  val ( >>= ) : 'a expr -> ('a -> 'b expr) -> 'b expr*)
val map : ?name:string -> ('a -> 'b) -> 'a expr -> 'b expr
val continue_with : ?name:string -> ('a -> 'b) -> 'a expr -> 'b expr
(*val ( let* ) : 'a expr -> ('a -> 'b expr) -> 'b expr*)
val ( let+ ) : 'a expr -> ('a -> 'b) -> 'b expr
val start_as_task : 'a expr -> 'a task
val start_with_continuation : ?name:string -> ('a -> 'b) -> 'a expr -> unit
val start : 'a expr -> unit
val await : 'a task -> 'a
val run_synchronously : 'a expr -> 'a

module Infix :
sig val ( >=> ) :'a expr ->  ('a -> 'b) -> unit end
