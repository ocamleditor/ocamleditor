type 'a t
val create : 'a option -> 'a t
val set : 'a t -> 'a -> unit
val reset : 'a t -> unit
val wait : 'a t -> 'a
