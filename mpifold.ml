open Printf

exception Unknown_tag of string

let tag_ready = 0
let tag_start = 1
let tag_done = 2
let tag_exit = 3
let tag_exception = 4

module type Foldable = sig
  type 'a t
  val foldi : 'a t -> init:'accum -> f:(int -> 'accum -> 'a -> 'accum) -> 'accum
end

module type State = sig
  type accum
  type b
  type t
  val init : accum -> t
  val get_accum : t -> accum
  val advance : t -> int * b -> t
  val continue : t -> bool
end

module Make (Foldable : Foldable) = struct
  let scheduler
      (type accum')
      ?(comm=Mpi.comm_world)
      t
      ~transform
      ~(init: accum')
      (module State : State with type accum = accum') =
    let myrank = Mpi.comm_rank comm in
    if myrank = 0 then (
      let rec fail children e =
        printf "%i\n%!" children;
        match children with
        | 0 -> raise e
        | _ ->
          let (_, rank, tag) = Mpi.receive_status Mpi.any_source Mpi.any_tag comm in
          if tag = tag_ready then (
            Mpi.send e rank tag_exception comm
          )
          else if tag = tag_done || tag = tag_exception then (
            let _ = Mpi.receive rank tag_ready comm in
            Mpi.send e rank tag_exception comm
          )
          else (
            raise (Unknown_tag "Received unknown tag during fail.")
          );
          fail (children - 1) e
      in
      let rec exit children state =
        match children with
        | 0 -> State.get_accum state
        | _ ->
          let (rank, tag) = Mpi.probe Mpi.any_source Mpi.any_tag comm in
          if tag = tag_ready then
            let () = Mpi.receive rank tag_ready comm in
            Mpi.send () rank tag_exit comm;
            exit (children -1) state
          else if tag = tag_done then (
            let transformed = Mpi.receive rank tag_done comm in
            let state = if State.continue state then
                State.advance state transformed
              else
                state
            in
            exit children state
          )
          else (
            let e = (Unknown_tag (sprintf "Rank %i received unknown tag: %i." myrank tag)) in
            fail children e
          )
      in
      let outer_loop children i state a =
        let rec loop state =
          let (rank, tag) =  Mpi.probe Mpi.any_source Mpi.any_tag comm in
          if tag = tag_ready then (
            let () = Mpi.receive rank tag_ready comm in
            Mpi.send (i, a) rank tag_start comm;
            state
          )
          else if tag = tag_done then (
            let transformed = Mpi.receive rank tag comm in
            let state = State.advance state transformed in
            if State.continue state then
              loop state
            else
              state
          )
          else if tag = tag_exception then (
            let (e : exn) = Mpi.receive rank tag_exception comm in
            fail (children - 1) e
          )
          else (
            let _ = Mpi.receive rank tag comm in
            let e = (Unknown_tag (sprintf "Rank %i received unknown tag: %i." myrank tag)) in
            fail children e
          )
        in
        if State.continue state then
          loop state
        else
          state
      in
      let children = Mpi.comm_size comm -1 in
      match children with
      | 0 -> failwith "No children nodes available."
      | _ ->
        let state = Foldable.foldi t ~init:(State.init init) ~f:(outer_loop children) in
        let accum = exit children state in
        Some accum
    )
    else (
      let rec child_loop () =
        Mpi.send () 0 tag_ready comm;
        let (_, tag) = Mpi.probe 0 Mpi.any_tag comm in
        if tag = tag_start then (
          let i, a = Mpi.receive 0 tag_start comm in
          let transformed= try
              transform i a
            with
            | e ->
              Mpi.send e 0 tag_exception comm;
              raise e
          in
          Mpi.send (i, transformed) 0 tag_done comm;
          child_loop ()
        )
        else if tag = tag_exit then (
          Mpi.receive 0 tag_exit comm |> ignore;
        )
        else if tag = tag_exception then (
          let e = Mpi.receive 0 tag_exception comm in
          raise e
        )
        else (
          let _ = Mpi.receive 0 tag comm in
          let e = (Unknown_tag (sprintf "Rank %i received unknown tag: %i." myrank tag)) in
          Mpi.send e 0 tag_exception comm;
          raise e
        )
      in
      child_loop ();
      None
    )

  let foldi (type b') (type accum') ?(comm=Mpi.comm_world) ?(ordered=true) t ~transform ~init ~f =
    let module State_delayed = struct
      module Int = struct
        type t = int
        let compare = (-)
      end
      module Buff = Map.Make (Int)
      type accum = accum'
      type b = b'
      type t = accum' * int * b' Buff.t
      let rec advance_buffer (accum, next, buff) =
        match Buff.find_opt next buff with
        | Some transformed ->
          let accum = f next accum transformed in
          let buff = Buff.remove next buff in
          advance_buffer (accum, (next + 1), buff)
        | None -> (accum, next, buff)
      let init accum = (accum, 0, Buff.empty)
      let get_accum (accum, _, _) = accum
      let advance (accum, next, buff) (i, t) =
        advance_buffer (accum, next, (Buff.add i t buff))
      let continue _ = true
    end
    in
    let module State_imediate = struct
      type accum = accum'
      type b = b'
      type t = accum'
      let init accum = accum
      let get_accum accum = accum
      let advance accum (i, t) = f i accum t
      let continue _ = true
    end
    in
    let s = if ordered then
        (module State_delayed : State with type accum = accum')
      else
        (module State_imediate : State with type accum = accum')
    in
    scheduler ~comm t ~transform ~init s

  let fold ?(comm=Mpi.comm_world) ?(ordered=true) t ~transform ~init ~f =
    foldi ~comm ~ordered t ~transform:(fun _ -> transform) ~init ~f:(fun _ -> f)

  let iteri ?(comm=Mpi.comm_world) t ~f =
    foldi ~comm ~ordered:false t ~transform:f ~init:() ~f:(fun _ () () -> ()) |> ignore

  let iter ?(comm=Mpi.comm_world) t ~f =
    foldi ~comm ~ordered:false t ~transform:(fun _ -> f) ~init:() ~f:(fun _ () () -> ()) |> ignore

  let existsi ?(comm=Mpi.comm_world) t ~f =
    let module State = struct
      type accum = bool
      type b = bool
      type t = bool
      let init accum = accum
      let get_accum accum = accum
      let advance accum (_, x) = accum || x
      let continue t = not t
    end
    in
    scheduler ~comm t ~transform:f ~init:false (module State : State with type accum = bool)

  let exists ?(comm=Mpi.comm_world) t ~f = existsi ~comm t ~f:(fun _ -> f)

  let for_alli ?(comm=Mpi.comm_world) t ~f =
    let module State = struct
      type accum = bool
      type b = bool
      type t = bool
      let init accum = accum
      let get_accum accum = accum
      let advance accum (_, x) = accum && x
      let continue t = t
    end
    in
    scheduler ~comm t ~transform:f ~init:true (module State : State with type accum = bool)

  let for_all ?(comm=Mpi.comm_world) t ~f = for_alli ~comm t ~f:(fun _ -> f)

  let findi (type a) ?(comm=Mpi.comm_world) t ~f =
    let module State = struct
      type accum = (int * a) option
      type b = a option
      type t = accum
      let init accum = accum
      let get_accum accum = accum
      let advance _ (i, x) = match x with
        | None -> None
        | Some x -> Some (i, x)
      let continue t = match t with
        | None -> true
        | Some _ -> false
    end
    in
    let transform i x =
      let cond = f i x in
      match cond with
      | true -> Some x
      | false -> None
    in
    let opt = scheduler ~comm t ~transform ~init:None (module State : State with type accum = (int * a) option) in
    match opt with
    | Some (Some x) -> Some x
    | _ -> None
      
  let find ?(comm=Mpi.comm_world) t ~f =
    match findi ~comm t ~f:(fun _ -> f) with
    | Some (_, x) -> Some x
    | None -> None
end
