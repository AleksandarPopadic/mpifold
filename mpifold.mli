module type Foldable = sig
  type 'a t
  val foldi : 'a t -> init:'accum -> f:(int -> 'accum -> 'a -> 'accum) -> 'accum
end

module Make (Foldable : Foldable) : sig
  (** [mpifold ?comm ?ordered t ~transform ~init ~f] returns
      [f (... f (f (f init (transform e1)) (transform e2)) (transform e3) ...)]
      on the parent node, where [e1..en] are the elements of t.
      The [transform] function is performed in parallel on children nodes.
      On children nodes, the function returns None. *)
  val mpifold
    :  ?comm:Mpi.communicator   (* default = Mpi.comm_world *)
    -> ?ordered:bool            (* default = true *)
    -> 'a Foldable.t
    -> transform:('a -> 'b)
    -> init:'accum
    -> f:('accum -> 'b -> 'accum)
    -> 'accum option

  (** Similar to [fold], except that [foldi] passes the index of each element to
      [f] and [transform] functions. *)
  val mpifoldi
    :  ?comm:Mpi.communicator   (* default = Mpi.comm_world *)
    -> ?ordered:bool            (* default = true *)
    -> 'a Foldable.t
    -> transform:(int -> 'a -> 'b)
    -> init:'accum
    -> f:(int -> 'accum -> 'b -> 'accum)
    -> 'accum option
end
