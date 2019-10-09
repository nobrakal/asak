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

  let lt x y =
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

let symmetric_difference x y =
  let rec aux x y =
    match x,y with
    | [],z|z,[] -> false,z
    | xx::xs,yy::ys ->
       match compare xx yy with
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
  in
  let b,res = aux x y in
  if b
  then Some res
  else None

let sum_of_fst xs = List.fold_left (fun acc (a,_) -> acc + a) 0 xs

let semimetric x y =
  let open Distance in
  match symmetric_difference x y with
  | None -> Infinity
  | Some diff -> Regular (sum_of_fst diff)

module Hash =
  struct
    type t = int * string
    let compare = compare
  end

module HashPairs =
  struct
    type t = Hash.t * Hash.t
    let compare = compare
  end

module HMap = Map.Make(HashPairs)
module HSet = Set.Make(Hash)

(* NB: the returned hashtable contains only keys (x,y) where x < y.
   This is not a problem since the distance is symmetric.
 *)
let compute_all_sym_diff cores xs =
  let len = List.length xs in
  let nb_per_cores = len / cores in
  let (_,xs,cored) =
    List.fold_left
      (fun (i,xs,acc) x ->
        if i mod nb_per_cores = 0
        then 1,[x],(xs::acc)
        else i+1,x::xs,acc) (1,[],[]) xs in
  let cored = xs::cored in
  let update_was_seen was_seen x y =
    HSet.add x (HSet.add y was_seen) in
  let aux ((x,xs),_) ((was_seen,res) as acc) ((y,ys),_) =
    if x < y
    then
      match semimetric xs ys with
      | Infinity -> acc
      | Regular dist ->
         update_was_seen was_seen x y,
         HMap.add (x,y) dist res
    else acc
  in
  let get_fst _ x _ = Some x in
  let neutral = (HSet.empty, HMap.empty) in
  let map xs' = List.fold_left (fun acc x -> List.fold_left (aux x) acc xs) neutral xs' in
  let fold (x1,y1) (x2,y2) = HSet.union x1 x2, HMap.union get_fst y1 y2 in
  List.fold_left fold neutral @@ Parmap.parmap ~ncores:cores ~chunksize:1  map (Parmap.L cored)

let dist semimetric x y =
  let rec aux x y =
    match x,y with
    | Leaf (x,_), Leaf (y,_) -> semimetric x y
    | Node (_,u,v), l | l, Node (_,u,v) ->
       let open Distance in
       match aux u l with
       | Infinity -> Infinity (* Avoid the computation of (aux v l) when possible *)
       | x -> max x (aux v l)
  in aux x y

let get_min_dist semimetric x y xs =
  List.fold_left
    (fun min x ->
      List.fold_left
        (fun min y ->
          let d = dist semimetric x y in
          if Distance.lt d (fst min)
          then (d,(x,y))
          else min
        ) min xs
    ) (dist semimetric x y, (x,y)) xs

let merge p u v xs =
  let xs = List.filter (fun x -> x != u && x != v) xs in
  (Node (p,u,v))::xs

module Elem = struct
  type t = (int * string) * (int * string) list
  let compare = compare
end

module Cluster = Map.Make(Elem)

let add_in_cluster map (x,xs) =
  match Cluster.find_opt xs map with
  | None -> Cluster.add xs [x] map
  | Some ys -> Cluster.add xs (x::ys) map

let remove_fst_in_tree t =
  fold_tree
    (fun p u v -> Node (p, u, v))
    (fun (_,x) -> Leaf x) t

let partition_map f g p l =
  let rec part yes no = function
  | [] -> (yes, no)
  | x :: l ->
     if p x then part (f x :: yes) no l else part yes (g x :: no) l in
  part [] [] l

let semimetric_from tbl x y =
  try
    let value =
      HMap.find (if x < y then (x,y) else (y,x)) tbl in
    Distance.Regular value
  with Not_found -> Distance.Infinity

let compute_with tbl =
  let rec compute = function
  | [] -> []
  | [x] -> [x]
  | x::y::_ as lst ->
     let (p, (u,v)) = get_min_dist (semimetric_from tbl) x y lst in
     match p with
     | Infinity -> lst
     | Regular p -> compute (merge p u v lst)
  in compute

let cluster cores (hash_list : ('a * ((int * string) * (int * string) list)) list) =
  let sorted_hash_list = List.rev_map (fun (x,(h,xs)) -> x,(h,List.sort compare (h::xs))) hash_list in
  let start =
    let cluster = List.fold_left add_in_cluster Cluster.empty sorted_hash_list in
    Cluster.fold (fun k xs acc -> (k,xs)::acc) cluster [] in
  let start,single =
    partition_map
      (fun x -> x) (fun (_,x) -> Leaf x) (fun ((_,xs),_) -> match xs with | [_] -> false | _ -> true) start in
  let was_seen,tbl = compute_all_sym_diff cores start in
  let (start, alone) =
    partition_map
      (fun ((x,_),xs) -> Leaf (x,xs)) (fun (_,x) -> Leaf x) (fun ((x,_),_) -> HSet.mem x was_seen) start in
  let dendrogram_list = compute_with tbl start in
  let cluster =
    List.sort
      (fun x y -> - compare (size_of_tree List.length x) (size_of_tree List.length y))
      (List.map remove_fst_in_tree dendrogram_list) in
  cluster @ alone @ single

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
