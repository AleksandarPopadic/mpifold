open Base
open Stdio

let myrank = Mpi.comm_rank Mpi.comm_world

let l = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]

(* Create the module with mpifold functions for lists. *)
module Mpifold_list = Mpifold.Make (List)

let result =
  Mpifold_list.fold ~ordered:false l
    (* The transform function is called on children nodes. *)
    ~transform:(fun x ->
        printf "%i: calculating f %i\n%!" myrank x;
        x * x
      )
    ~init:0
    (* The folding function is called on the parent node, receiving
       values from children as they are calculated. *)
    ~f:(fun accum x ->
        printf "%i: fold received %i\n%!" myrank x;
        accum + x  
      )

(* The result is significant only on node 0. *)
let () =
    match result with
      | Some result -> printf "%i: final result is %i\n%!" myrank result
      | _ -> printf "%i: None\n%!" myrank
             
