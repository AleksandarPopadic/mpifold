module type Foldable = sig
  type 'a t
  val foldi : 'a t -> init:'accum -> f:(int -> 'accum -> 'a -> 'accum) -> 'accum
end

module Make (Foldable : Foldable) : sig
  (** [map_fold ?comm ?ordered t ~map ~init ~f] returns
      [f (... f (f (f init (map e1)) (map e2)) (map e3) ...)]  on the
      parent node, where [e1..en] are the elements of t. The [map]
      function is performed in parallel on children nodes.
      On children nodes, the function returns None. *)
  val map_fold
    :  ?comm:Mpi.communicator   (* default = Mpi.comm_world *)
    -> ?ordered:bool            (* default = true *)
    -> 'a Foldable.t
    -> map:('a -> 'b)
    -> init:'accum
    -> f:('accum -> 'b -> 'accum)
    -> 'accum option

  (** Similar to [map_fold], except that [map_foldi] also passes the index of
      each element to [f] and [map] functions. *)
  val map_foldi
    :  ?comm:Mpi.communicator   (* default = Mpi.comm_world *)
    -> ?ordered:bool            (* default = true *)
    -> 'a Foldable.t
    -> map:(int -> 'a -> 'b)
    -> init:'accum
    -> f:(int -> 'accum -> 'b -> 'accum)
    -> 'accum option

  (** [iter ?comm t f] applies function f to elements of t. *)
  val iter
    :  ?comm:Mpi.communicator   (* default = Mpi.comm_world *)
    -> 'a Foldable.t
    -> f:('a -> unit)
    -> unit

  (** Similar to [iter], except thath [iteri] also passes the index of
      each element to the iterated function. *)
  val iteri
    :  ?comm:Mpi.communicator   (* default = Mpi.comm_world *)
    -> 'a Foldable.t
    -> f:(int -> 'a -> unit)
    -> unit

  (** Returns true if and only if there exists an element for which
      the provided function evaluates to true. *)
  val exists
    :  ?comm:Mpi.communicator   (* default = Mpi.comm_world *)
    -> 'a Foldable.t
    -> f:('a -> bool)
    -> bool option

  (** Similar to [exists], except thath [existsi] also passes the
      index of each element to the provided function. *)
  val existsi
    :  ?comm:Mpi.communicator   (* default = Mpi.comm_world *)
    -> 'a Foldable.t
    -> f:(int -> 'a -> bool)
    -> bool option

  (** Returns true if and only if the provided function evaluates to
      true for all elements. *)
  val for_all
    :  ?comm:Mpi.communicator   (* default = Mpi.comm_world *)
    -> 'a Foldable.t
    -> f:('a -> bool)
    -> bool option

  (** Similar to [for_all], except thath [for_alli] also passes the
      index of each element to the provided function. *)
  val for_alli
    :  ?comm:Mpi.communicator   (* default = Mpi.comm_world *)
    -> 'a Foldable.t
    -> f:(int -> 'a -> bool)
    -> bool option

  (** Returns as an option the first element for which f evaluates to
      true. *)
  val find
    :  ?comm:Mpi.communicator   (* default = Mpi.comm_world *)
    -> 'a Foldable.t
    -> f:('a -> bool)
    -> 'a option

  (** Similar to [find], except thath [findi] also passes the index of
      each element to the provided function. *)
  val findi
    :  ?comm:Mpi.communicator   (* default = Mpi.comm_world *)
    -> 'a Foldable.t
    -> f:(int -> 'a -> bool)
    -> (int * 'a) option
end
