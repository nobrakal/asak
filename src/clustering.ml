(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Wtree

let compare_snd (_,x) (_,y) = compare x y

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

let symmetric_difference cmp =
  let rec aux x y =
    match x,y with
    | [],z|z,[] -> false,z
    | xx::xs,yy::ys ->
       match cmp xx yy with
       | (-1) ->
          let b,ndiff = aux xs y in
          b,xx::ndiff
       | 0 ->
          let _,ndiff = aux xs ys in
          true,ndiff
       | 1 ->
          let b,ndiff = aux x ys in
          b,yy::ndiff
       | _ -> failwith "symmetric_difference"
  in aux

let sum_of_fst = List.fold_left (fun acc (a,_) -> acc + a) 0

let dist x y =
  let open Distance in
  let rec aux x y =
    match x,y with
    | Leaf (x,_), Leaf (y,_) ->
       let b,diff = symmetric_difference compare_snd x y in
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
          then  min := Some (choose_option (dist x y) (x,y) !min)
        )
        xs
    ) xs;
  match !min with
  | None -> failwith "get_min_dist"
  | Some x -> x

let merge p u v xs =
  let xs = List.filter (fun x -> x != u && x != v) xs in
  (Node (p,u,v))::xs

module Elem = struct
  type t = (int * string) list
  let compare = compare
end

module Cluster = Map.Make(Elem)

(* Add x in a cluster, identified by its hash list xs *)
let add_in_cluster map (x,xs) =
  match Cluster.find_opt xs map with
  | None -> Cluster.add xs [x] map
  | Some ys -> Cluster.add xs (x::ys) map

let remove_fst_in_tree t =
  fold_tree
    (fun p u v -> Node (p, u, v))
    (fun (_,x) -> Leaf x) t

let cluster (hash_list : ('a * (int * string) list) list) =
  let rec aux = function
    | [] -> []
    | [x] -> [x]
    | lst ->
        let (p, (u,v)) = get_min_dist lst in
        match p with
        | Infinity -> lst
        | Regular p -> aux (merge p u v lst) in
  let start =
    let cluster = List.fold_left add_in_cluster Cluster.empty hash_list in
    Cluster.fold (fun k xs acc -> Leaf (k, xs)::acc) cluster [] in
  List.sort
    (fun x y -> - compare (size_of_tree List.length x) (size_of_tree List.length y)) @@
    List.map remove_fst_in_tree @@
      aux start

let print_cluster show cluster =
  let rec aux i = function
    | Leaf x ->
       begin
         print_string (String.make i ' ');
         List.iter (fun x -> print_string (show x ^ " ")) x;
         print_endline "";
       end
    | Node (w,x,y) ->
       begin
         print_string (String.make i ' ');
         print_string ("Node " ^ string_of_int w ^":");
         print_endline "";
         aux (i+1) x;
         aux (i+1) y
       end
  in
  let pclass i x =
    print_endline ("Class " ^ string_of_int i ^ ":");
    aux 1 x
  in List.iteri pclass cluster
