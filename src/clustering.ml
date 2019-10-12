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

module HPMap = Map.Make(HashPairs)
module HMap = Map.Make(Hash)
module HSet = Set.Make(Hash)

let split_by_cores cores xs =
  let len = List.length xs in
  let nb_per_cores = max 1 (len / cores) in
  let (_,left,cored) =
    List.fold_left
      (fun (i,xs,acc) x ->
        if i mod nb_per_cores = 0
        then 1,[x],(xs::acc)
        else i+1,x::xs,acc) (1,[],[]) xs in
  left::cored

let add_if_non_inf (x,xs) ((s,m) as acc) (y,ys) =
  if x < y
  then
    match semimetric xs ys with
    | Infinity -> acc
    | Regular dist ->
       HSet.add x (HSet.add y s),
       HPMap.add (x,y) dist m
  else acc

(* NB: the returned hashtable contains only keys (x,y) where x < y.
   This is not a problem since the distance is symmetric.
 *)
let compute_all_sym_diff cores xs =
  let cored = split_by_cores cores xs in
  let get_fst _ x _ = Some x in
  let neutral = (HSet.empty, HPMap.empty) in
  let map xs' =
    List.fold_left
      (fun acc x -> List.fold_left (add_if_non_inf x) acc xs) neutral xs' in
  let fold (s1,m1) (s2,m2) = HSet.union s1 s2, HPMap.union get_fst m1 m2 in
  List.fold_left fold neutral @@
    Parmap.parmap ~ncores:cores ~chunksize:1 map (Parmap.L cored)

let dist semimetric x y =
  let rec aux x y =
    match x,y with
    | Leaf x, Leaf y -> semimetric x y
    | Node (_,u,v), l | l, Node (_,u,v) ->
       let open Distance in
       match aux u l with
       | Infinity -> Infinity (* Avoid the computation of (aux v l) when possible *)
       | x -> max x (aux v l)
  in aux x y

let get_min_dist semimetric x y xs =
  let xs = x::y::xs in
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

let semimetric_from tbl x y =
  try
    let value =
      HPMap.find (if x < y then (x,y) else (y,x)) tbl in
    Distance.Regular value
  with Not_found -> Distance.Infinity

let iter_on_cart_prod f xs =
  List.iter (fun x -> List.iter (f x) xs) xs

let classes_of_uf xs =
  let m =
    List.fold_left
      (fun m (x',x) ->
        let x = UnionFind.get x in
        try
          let xs = HMap.find x m in
          HMap.add x (x'::xs) m
        with | Not_found -> HMap.add x [x'] m
      ) HMap.empty xs in
  HMap.fold (fun _ xs acc -> xs::acc) m []

(* Surapproximate classes by using the transitive closure of xRy <=> dist x y < Infinity.
   If not (xRy) => dist x y = Infinity, thus x and y cannot be in the same class.
 *)
let surapproximate_classes tbl xs =
  let xs' = List.map (fun x -> x, UnionFind.make x) xs in
  let try_to_merge (x',x) (y',y) =
    if x' < y'
    then
      if HPMap.mem (x',y') tbl
      then let _ = UnionFind.union x y in () in
  iter_on_cart_prod try_to_merge xs';
  classes_of_uf xs'

(* Compute a hierarchical clustering *)
let refine_class tbl (xs : Hash.t wtree list) =
  let rec compute xs =
    match xs with
    | [] | [_] -> xs
    | x::y::_  ->
       let (p, (u,v)) = get_min_dist (semimetric_from tbl) x y xs in
       match p with
       | Infinity -> xs
       | Regular p -> compute (merge p u v xs)
  in compute xs

let hierarchical_clustering cores tbl (xs : Hash.t wtree list list) =
  let cored = split_by_cores cores xs in
  let rafine_class = List.fold_left (fun acc x -> List.rev_append (refine_class tbl x) acc) [] in
  Parmap.parmapfold ~ncores:cores ~chunksize:1
    rafine_class (Parmap.L cored) List.rev_append [] List.rev_append

let add_in_cluster map (x,(h,xs)) =
  match HMap.find_opt h map with
  | None -> HMap.add h (xs,[x]) map
  | Some (_,ys) -> HMap.add h (xs,x::ys) map

let create_start_cluster sorted_hash_list =
  let cluster = List.fold_left add_in_cluster HMap.empty sorted_hash_list in
  HMap.fold (fun k xs acc -> (k,xs)::acc) cluster []

let compare_size_of_trees x y =
  compare (size_of_tree List.length x) (size_of_tree List.length y)

let cluster cores (hash_list : ('a * (Hash.t * Hash.t list)) list) =
  let sorted_hash_list =
    List.rev_map (fun (x,(h,xs)) -> x,(h,List.sort compare (h::xs))) hash_list in
  let start = create_start_cluster sorted_hash_list in
  let start,assoc_hash_ident_list =
    List.fold_left
      (fun (acc,m) (main_hash,(lst,xs)) -> (main_hash,lst)::acc,HMap.add main_hash xs m)
      ([],HMap.empty) start in
  let create_leaf k = Leaf (HMap.find k assoc_hash_ident_list) in
  let was_seen,distance_matrix = compute_all_sym_diff cores start in
  let lst = List.map fst start in
  let lst,alone = List.partition (fun x -> HSet.mem x was_seen) lst in
  let surapprox = surapproximate_classes distance_matrix lst in
  let surapprox = List.map (List.map (fun x -> Leaf x)) surapprox in
  let dendrogram_list = hierarchical_clustering cores distance_matrix surapprox in
  let cluster =
    List.sort
      (fun x y -> - (compare_size_of_trees x y))
      (List.map (fold_tree (fun a b c -> Node (a,b,c)) create_leaf) dendrogram_list) in
  cluster @ (List.rev_map create_leaf alone)

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
