(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Wtree

module Distance = struct

  type t = Regular of int | Infinity

  let compare x y =
    match x with
    | Infinity -> 1
    | Regular x' ->
       match y with
       | Infinity -> -1
       | Regular y' -> compare x' y'

  let ( < ) x y =
    compare x y = -1

  let max x y =
    if compare x y <= 0
    then y
    else x

  let min x y =
    if compare x y <= 0
    then x
    else y
end

let rec symmetric_difference x y =
  match x,y with
  | [],z|z,[] -> false,z
  | xx::xs,yy::ys ->
     if xx < yy
     then let b,ndiff = symmetric_difference xs y in
          b,xx::ndiff
     else
       if xx > yy
       then let b,ndiff = symmetric_difference x ys in
            b,yy::ndiff
       else let _,ndiff = symmetric_difference xs ys in
            true,ndiff

let sum_of_fst = List.fold_left (fun acc (a,_) -> acc + a) 0

let dist x y =
  let open Distance in
  let rec aux x y =
    match x,y with
    | Leaf (x,_), Leaf (y,_) ->
       let b,diff = symmetric_difference x y in
       if b
       then Regular (sum_of_fst diff)
       else Infinity
    | Node (_,u,v), l | l, Node (_,u,v) ->
       max (aux u l) (aux v l)
  in aux x y

let get_min_dist xs =
  let choose_option d e =
    let open Distance in function
    | None -> (d,e)
    | Some (old_d,old_e) ->
       if d < old_d
       then (d,e)
       else (old_d,old_e) in
  let min = ref None in
  List.iter
    (fun x ->
      List.iter (fun y ->
          if x != y
          then
            min := Some (choose_option (dist x y) (x,y) !min)
        )
        xs
    ) xs;
  match !min with
  | None -> failwith "get_min_dist"
  | Some x -> x


let merge p u v xs =
  let xs = List.filter (fun x -> x != u && x != v) xs in
  (Node (p,u,v))::xs

(* Add x in a cluster, identified by its hash list xs *)
let add_in_cluster x xs =
  let rec go = function
    | [] -> [(xs,[x])]
    | ((us,ys) as e)::zs ->
       if us = xs
       then (us,x::ys)::zs
       else e::go zs
  in go

let remove_fst_in_tree t =
  fold_tree
    (fun p u v -> Node (p, u, v))
    (fun (_,x) -> Leaf x) t

(* Compute a hierarchical cluster from data *)
let cluster (m : ('a * (int * string) list) list) : ('a list) wtree list =
  let rec aux = function
    | [] -> []
    | [x] -> [x]
    | lst ->
       let (p, (u,v)) = get_min_dist lst in
       match p with
       | Infinity -> lst
       | Regular p -> aux (merge p u v lst)
  in
  let start =
    List.map (fun x -> Leaf x) @@
      List.fold_left
        (fun acc (x,xs) -> add_in_cluster x (List.sort compare xs) acc) [] m
  in
  List.sort
    (fun x y -> - compare (size_of_tree List.length x) (size_of_tree List.length y)) @@
    List.map remove_fst_in_tree @@
      aux start
