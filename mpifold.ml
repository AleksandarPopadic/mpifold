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

module Make (Foldable : Foldable) = struct
  let mpifoldi ?(comm=Mpi.comm_world) ?(ordered=true) t ~transform ~init ~f =
    let myrank = Mpi.comm_rank comm in
    if myrank = 0 then (
      let module Buffer = struct
        module Int = struct
          type t = int
          let compare = (-)
        end
        module Buff = Map.Make (Int)
        let rec advance_buffer ((accum, next, buff) as buffer)=
          match Buff.find_opt next buff with
          | Some transformed ->
            let accum = f next accum transformed in
            let buff = Buff.remove next buff in
            advance_buffer (accum, next + 1, buff)
          | None -> buffer
        let empty accum = (accum, 0, Buff.empty)
        let add (accum, next, buff) (i, t) = match ordered with
          | true -> advance_buffer (accum, next, Buff.add i t buff)
          | false -> (f i accum t, next, buff)
      end
      in
      let rec fail children e =
        printf "%i\n%!" children;
        match children with
        | 0 -> raise e
        | _ ->
          let (_, rank, tag) = Mpi.receive_status Mpi.any_source Mpi.any_tag Mpi.comm_world in
          if tag = tag_ready then (
            Mpi.send e rank tag_exception Mpi.comm_world
          )
          else if tag = tag_done || tag = tag_exception then (
            let _ = Mpi.receive rank tag_ready Mpi.comm_world in
            Mpi.send e rank tag_exception Mpi.comm_world
          )
          else (
            raise (Unknown_tag "Received unknown tag during fail.")
          );
          fail (children - 1) e
      in
      let rec exit children buffer =
        match children with
        | 0 ->
          let (accum, _, _) = Buffer.advance_buffer buffer in
          accum
        | _ ->
          let (rank, tag) = Mpi.probe Mpi.any_source Mpi.any_tag Mpi.comm_world in
          if tag = tag_ready then
            let () = Mpi.receive rank tag_ready Mpi.comm_world in
            Mpi.send () rank tag_exit Mpi.comm_world;
            exit (children -1) buffer
          else if tag = tag_done then (
            let transformed = Mpi.receive rank tag_done Mpi.comm_world in
            exit children (Buffer.add buffer transformed)
          )
          else (
            let e = (Unknown_tag (sprintf "Rank %i received unknown tag: %i." myrank tag)) in
            fail children e
          )
      in
      let outer_loop children i buffer a =
        let rec loop buffer =
          let (rank, tag) =  Mpi.probe Mpi.any_source Mpi.any_tag Mpi.comm_world in
          if tag = tag_ready then (
            let () = Mpi.receive rank tag_ready Mpi.comm_world in
            Mpi.send (i, a) rank tag_start Mpi.comm_world;
            buffer
          )
          else if tag = tag_done then (
            let transformed = Mpi.receive rank tag Mpi.comm_world in
            let buffer = Buffer.add buffer transformed in
            loop buffer
          )
          else if tag = tag_exception then (
            let (e : exn) = Mpi.receive rank tag_exception Mpi.comm_world in
            fail (children - 1) e
          )
          else (
            let _ = Mpi.receive rank tag Mpi.comm_world in
            let e = (Unknown_tag (sprintf "Rank %i received unknown tag: %i." myrank tag)) in
            fail children e
          )
        in
        loop buffer
      in
      let children = Mpi.comm_size comm -1 in
      match children with
      | 0 -> failwith "No children nodes available."
      | _ ->
        let buffer = Foldable.foldi t ~init:(Buffer.empty init) ~f:(outer_loop children) in
        let accum = exit children buffer in
        Some accum
    )
    else (
      let rec child_loop () =
        Mpi.send () 0 tag_ready Mpi.comm_world;
        let (_, tag) = Mpi.probe 0 Mpi.any_tag Mpi.comm_world in
        if tag = tag_start then (
          let i, a = Mpi.receive 0 tag_start Mpi.comm_world in
          let transformed= try
              transform i a
            with
            | e ->
              Mpi.send e 0 tag_exception Mpi.comm_world;
              raise e
          in
          Mpi.send (i, transformed) 0 tag_done Mpi.comm_world;
          child_loop ()
        )
        else if tag = tag_exit then (
          Mpi.receive 0 tag_exit Mpi.comm_world |> ignore;
        )
        else if tag = tag_exception then (
          let e = Mpi.receive 0 tag_exception Mpi.comm_world in
          raise e
        )
        else (
          let _ = Mpi.receive 0 tag Mpi.comm_world in
          let e = (Unknown_tag (sprintf "Rank %i received unknown tag: %i." myrank tag)) in
          Mpi.send e 0 tag_exception Mpi.comm_world;
          raise e
        )
      in
      child_loop ();
      None
    )

  let mpifold ?(comm=Mpi.comm_world) ?(ordered=true) t ~transform ~init ~f =
    mpifoldi ~comm ~ordered t ~transform:(fun _ -> transform) ~init~f:(fun _ -> f)
end
