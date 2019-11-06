(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

module Wtree = struct
  type 'a wtree =
    | Node of (int * 'a wtree * 'a wtree)
    | Leaf of 'a

  let fold_tree n l =
    let rec aux = function
      | Leaf a -> l a
      | Node (f,a,b) -> n f (aux a) (aux b)
    in aux

  let size_of_tree f =
    fold_tree (fun _ a b -> 1 + a + b) f
end
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
    compare x y < 0

  let max x y =
    if compare x y <= 0
    then y
    else x

  let min x y =
    if compare x y <= 0
    then x
    else y
end

(** symmetric difference between sorted sets *)
let symmetric_difference x y =
  let rec aux x y =
    match x,y with
    | [],z|z,[] -> false,z
    | xx::xs,yy::ys ->
       let cmp = compare xx yy in
       if cmp < 0 then
         let b,ndiff = aux xs y in
         b,xx::ndiff
       else if cmp > 0 then
          let b,ndiff = aux x ys in
          b,yy::ndiff
       else
          let _,ndiff = aux xs ys in
          true,ndiff
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

let add_if_non_inf assoc_main_subs x ((s,m) as acc) y =
  if x < y
  then
    match semimetric (Hashtbl.find assoc_main_subs x) (Hashtbl.find assoc_main_subs y) with
    | Infinity -> acc
    | Regular dist ->
       HSet.add x (HSet.add y s),
       HPMap.add (x,y) dist m
  else acc

(* NB: the returned hashtable contains only keys (x,y) where x < y.
   This is not a problem since the distance is symmetric.
 *)
let compute_all_sym_diff assoc_main_subs xs =
  List.fold_left
    (fun acc x -> List.fold_left (add_if_non_inf assoc_main_subs x) acc xs)
    (HSet.empty, HPMap.empty)
    xs

let hashtbl_update table ~default k f =
  match Hashtbl.find table k with
    | exception Not_found -> Hashtbl.add table k (f default)
    | v -> Hashtbl.replace table k (f v)

let compute_all_sym_diff_fast children xs =
  (* For each pair of parent nodes, we want to compute the sum of
     weights of the children in their symmetric difference.

     The general idea of this algorithm is to compute, for a given
     parent, the distance to all its "neighbors" (the parents with
     some children in common) in one traversal of its children. During
     this traversal we compute, for each neighbor of this parent, the
     sum of the weights of the children they have in common. From this
     "common weight" we can deduce the weight of the symmetric
     difference.
 *)
  let parents =
    (* map each subtree to the list of its parents *)
    let parents = Hashtbl.create 42 in
    let add_child ~parent (_weight, child) = Hashtbl.add parents child parent in
    let add_children parent =
      List.iter (add_child ~parent) (Hashtbl.find children parent) in
    List.iter add_children xs; parents in
  let increment_key table key value =
    hashtbl_update table ~default:0 key (fun n -> n + value) in
  let total_weights =
    (* map each tree to the sum of its children weights *)
    let weights = Hashtbl.create 42 in
    let add_child_weight ~parent (weight, _child) =
      increment_key weights parent weight in
    let add_weights parent =
      List.iter (add_child_weight ~parent) (Hashtbl.find children parent) in
    List.iter add_weights xs;
    weights in
  let node_neighbors x =
    (* map each node to a hashtable of the common weights with its neighbors *)
    let diffs = Hashtbl.create 42 in
    let add_child ~parent (weight, child) =
      let add_common_weight neighbor =
        if not (parent < neighbor) then ()
        else increment_key diffs neighbor weight in
      List.iter add_common_weight (Hashtbl.find_all parents child) in
    List.iter (add_child ~parent:x) (Hashtbl.find children x);
    diffs
  in
  let nodes = ref HSet.empty in
  let dists = ref HPMap.empty in
  let treat_node x =
    nodes := HSet.add x !nodes;
    let add_neighbor y common_weight =
      let dist =
        Hashtbl.find total_weights x
        + Hashtbl.find total_weights y
        - 2 * common_weight in
      dists := HPMap.add (x, y) dist !dists in
    Hashtbl.iter add_neighbor (node_neighbors x) in
  List.iter treat_node xs;
  !nodes, !dists

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
      Hashtbl.find tbl (if x < y then (x,y) else (y,x)) in
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
      if Hashtbl.mem tbl (x',y')
      then let _ = UnionFind.union x y in () in
  iter_on_cart_prod try_to_merge xs';
  classes_of_uf xs'

let adjacency_lists tbl xs =
  let neighbors = Hashtbl.create (List.length xs) in
  let add_link x y =
    if x < y && Hashtbl.mem tbl (x, y) then begin
        Hashtbl.add neighbors x y;
        Hashtbl.add neighbors y x;
      end in
  iter_on_cart_prod add_link xs;
  neighbors

(* Surapproximate classes by using the transitive closure of xRy <=> dist x y < Infinity.
   If not (xRy) => dist x y = Infinity, thus x and y cannot be in the same class.
 *)
let surapproximate_classes_nouf neighbors xs =
  let classes = ref [] in
  let missing_nodes = ref (List.fold_right HSet.add xs HSet.empty) in
  while not (HSet.is_empty !missing_nodes) do
    let start = HSet.choose !missing_nodes in
    let to_visit = Stack.create () in
    Stack.push start to_visit;
    let new_class = ref HSet.empty in
    while not (Stack.is_empty to_visit) do
      let cur = Stack.pop to_visit in
      if HSet.mem cur !new_class then ()
      else begin
          new_class := HSet.add cur !new_class;
          List.iter (fun next -> Stack.push next to_visit) (Hashtbl.find_all neighbors cur)
        end
    done;
    missing_nodes := HSet.diff !missing_nodes !new_class;
    classes := HSet.elements !new_class :: !classes;
  done;
  !classes

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

let hierarchical_clustering tbl (xs : Hash.t wtree list list) =
  List.fold_left (fun acc x -> List.rev_append (refine_class tbl x) acc) [] xs

let add_in_cluster map (x,h) =
  match HMap.find_opt h map with
  | None -> HMap.add h [x] map
  | Some ys -> HMap.add h (x::ys) map

let create_start_cluster hash_list =
  let cluster = List.fold_left add_in_cluster HMap.empty hash_list in
  HMap.fold (fun k xs acc -> (k,xs)::acc) cluster []

let compare_size_of_trees x y =
  compare (size_of_tree List.length x) (size_of_tree List.length y)

let convert_map_to_hm m =
  let size = HPMap.cardinal m in
  let ht = Hashtbl.create size in
  HPMap.iter (Hashtbl.add ht) m;
  ht

let add_in_assoc tbl (_,(h,xs)) =
  if not (Hashtbl.mem tbl h)
  then Hashtbl.add tbl h (List.sort compare xs)

let cluster ?filter_small_trees (hash_list : ('a * (Hash.t * Hash.t list)) list) =
  let last = ref (Unix.gettimeofday ()) in
  let time () =
    let now = Unix.gettimeofday () in
    let old = !last in
    last := now;
    now -. old in
  let debug msg =
    Printf.eprintf "[%12f] %s\n%!" (time ()) msg in
  let hash_list =
    match filter_small_trees with
    | None -> hash_list
    | Some t -> List.filter (fun (_,((p,_),_)) -> p >= t) hash_list in
  let assoc_main_subs = Hashtbl.create (List.length hash_list) in
  List.iter (add_in_assoc assoc_main_subs) hash_list;
  let start = create_start_cluster (List.rev_map (fun (k,(h,_)) -> k,h) hash_list) in
  let start,assoc_hash_ident_list =
    List.fold_left
      (fun (acc,m) (main_hash,xs) -> main_hash::acc,HMap.add main_hash xs m)
      ([],HMap.empty) start in
  let create_leaf k = Leaf (HMap.find k assoc_hash_ident_list) in
  debug "start";
  let was_seen,distance_matrix = compute_all_sym_diff assoc_main_subs start in
  debug "compute_all_sym_diff done";
  let was_seen_fast,distance_matrix_fast = compute_all_sym_diff_fast assoc_main_subs start in
  debug "compute_all_sym_diff_fast done";
  let check x y =
    assert (HSet.mem x was_seen_fast);
    assert (HSet.mem y was_seen_fast);
    assert (HPMap.find_opt (x, y) distance_matrix
            = HPMap.find_opt (x, y) distance_matrix_fast);
  in
  iter_on_cart_prod check (HSet.elements was_seen);
  debug "check compute_all_sym_diff";
  let hdistance_matrix = convert_map_to_hm distance_matrix in
  debug "convert_map_to_hm done";
  let lst,alone = List.partition (fun x -> HSet.mem x was_seen) start in
  let surapprox = surapproximate_classes hdistance_matrix lst in
  debug "surapproximate_classes done";
  let neighbors = adjacency_lists hdistance_matrix lst in
  debug "adjacency_list done";
  let surapprox_nouf = surapproximate_classes_nouf neighbors lst in
  debug "surapproximate_classes_nouf done";
  let check classes classes' =
    let hset li = List.fold_right HSet.add li HSet.empty in
    let classes, classes' = List.map hset classes, List.map hset classes' in
    let sort classes =
      List.sort (fun cl1 cl2 -> compare (HSet.min_elt cl1) (HSet.min_elt cl2)) classes in
    let classes, classes' = sort classes, sort classes' in
    assert (List.for_all2 HSet.equal classes classes') in
  check surapprox surapprox_nouf;
  debug "check surapproximate_classes";
  let surapprox = List.map (List.map (fun x -> Leaf x)) surapprox in
  let dendrogram_list = hierarchical_clustering hdistance_matrix surapprox in
  debug "hierarchical_clustering done";
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

let fake_inputs ~set_size ~cluster_size ~cluster_nb =
  let fake_string () = String.init 40 (fun _ -> char_of_int (Random.int 256)) in
  let fake_hash () = Digest.string (fake_string ()) in

  let fake_hashes () =
    let total_weight = ref 0 in
    let hashes =
      List.init set_size (fun _ ->
          let weight = Random.int set_size in
          total_weight := weight + !total_weight;
          weight, fake_hash ()) in
    (!total_weight, fake_hash ()), hashes in

  let mutate_hashes ((weight, _hash), hashlist) =
    let mutate (w, hash) =
      if Random.bool () then (w, hash) else (w, fake_hash ()) in
    ((weight, fake_hash ()), List.map mutate hashlist) in

  let seeds = List.init cluster_nb (fun _ -> fake_hashes ()) in
  let from_seed seed =
    seed :: List.init cluster_size (fun _ -> mutate_hashes seed) in
  List.concat (List.map from_seed seeds)

let () =
  prerr_endline "usage: ./foo set_size cluster_size cluster_nb";
  let set_size = int_of_string Sys.argv.(1) in
  let cluster_size = int_of_string Sys.argv.(2) in
  let cluster_nb = int_of_string Sys.argv.(3) in
  let inputs = fake_inputs ~set_size ~cluster_size ~cluster_nb
               |> List.map (fun hashes -> (Digest.to_hex (snd (fst hashes))), hashes) in
  let result = cluster inputs in
  (* print_cluster (fun x -> x) result; *)
  ignore result

(*
ocamlfind ocamlopt -package unix,unionFind -linkpkg -o test clustering_bench.ml \
&& time ./test 40 100 10
*)
