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

(* code from the OCaml manual (tutorial on modules) *)
module PrioQueue =
  struct
    type priority = int
    type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
    let empty = Empty
    let is_empty = function
      | Empty -> true
      | Node _ -> false
    let rec insert queue prio elt =
      match queue with
        Empty -> Node(prio, elt, Empty, Empty)
      | Node(p, e, left, right) ->
          if prio <= p
          then Node(prio, elt, insert right p e, left)
          else Node(p, e, insert right prio elt, left)
    exception Queue_is_empty
    let top = function
      | Empty -> raise Queue_is_empty
      | Node(prio, elt, _, _) -> (prio, elt)
    let rec remove_top = function
        Empty -> raise Queue_is_empty
      | Node(_prio, _elt, left, Empty) -> left
      | Node(_prio, _elt, Empty, right) -> right
      | Node(_prio, _elt, (Node(lprio, lelt, _, _) as left),
                          (Node(rprio, relt, _, _) as right)) ->
          if lprio <= rprio
          then Node(lprio, lelt, remove_top left, right)
          else Node(rprio, relt, left, remove_top right)
    let extract = function
        Empty -> raise Queue_is_empty
      | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
  end;;


let hierarchical_clustering_fast tbl classes =
  let cluster connex_class =
    (* Within each connected component, we can expect most nodes to
       have a distance to each other. We store nodes as dense arrays
       by indexing the input leaves from 0 to N-1, and we store
       distances between nodes in a dense matrix.

       At most N-1 non-leaf nodes will be created by the clustering process.
       (One way to prove this is to look at the number of nodes that are "roots"
       (do not have a parent) at any point in the construction. At the beginning,
       the N leaves are all roots. Each time we add a new node, its children were
       roots and are not anymore, so have one less root. At the end, there is at least
       one root.)
       So we index all intermediate trees in the construction from 0 to 2N-2, starting
       with the initial leaves, and filling the end of those arrays/matrices with nodes
       as they constructed.

       The fast algorithm keeps the following state:
       - [nodes]: a mapping from indices to trees
       - [roots]: a boolean array to know which trees are roots at the current step
       - [distances]: a matrix of distances between all known nodes
       - [next_slot]: the index of the next node to be constructed
       - [queue]: a priority queue of pairs of nodes,

       At a given step, only the indices from [0] to [!next_slot - 1] represent valid trees,
       so the values in [roots], [distances] and [nodes] above [!next_slot] are invalid.

       Because distances are symmetric, we store the distance between indices [i] and [j]
       in [distances.(min i j).(max i j)].
       (We could store only a diagonal matrix to save space.)
     *)
    let size = List.length connex_class in (* N *)
    let class_array = Array.of_list connex_class in
    let nodes =
      List.map (fun x -> Leaf x) connex_class
      @ List.init (size - 1) (fun _ -> Leaf (List.hd connex_class))
      |> Array.of_list in
    let roots = Array.make (2 * size - 1) true in
    let queue = ref PrioQueue.empty in
    let distances =
      (* construct the initial distance matrix between leaves,
         and initialize [queue] with the N^2 pairs of leaves *)
      let dist_leaves i j =
        let x, y = class_array.(i), class_array.(j) in
        match Hashtbl.find tbl (if x < y then (x, y) else (y, x)) with
          | dist ->
             queue := PrioQueue.insert !queue dist (i, j);
             Distance.Regular dist
          | exception Not_found -> Distance.Infinity in
      Array.init (2 * size - 1) (fun i -> Array.init (2 * size - 1) (fun j ->
        if i < size && j < size && i < j
        then dist_leaves i j
        else Distance.Regular (-1))) in
    let next_slot = ref size in
    while not (PrioQueue.is_empty !queue || !next_slot = 2 * size - 1) do
      let (dist, (i, j), rest) = PrioQueue.extract !queue in
      (* We use the priority queue to select the pair of closest nodes.
         We can only pair those nodes if they are not already roots,
         we skip all the pairs that contain a non-root node. *)
      queue := rest;
      assert (i <> j);
      if not (roots.(i) && roots.(j)) then ()
      else begin
          let x, y = nodes.(i), nodes.(j) in
          let k = !next_slot in
          assert (k < 2 * size - 1);
          nodes.(k) <- Node(dist, x, y);
          incr next_slot;
          roots.(i) <- false;
          roots.(j) <- false;
          assert roots.(k);
          (* We create a new node [k], whose children [i] and [j] are not roots anymore.

             (this may invalidate many pairs in [queue], but they stay there and
              will be discarded when picked; [queue] is large so cleaning it up
              would be too costly)

             We now compute the distance between [k] and all older root
             nodes (indices upto [k - 1]). At the same time we add each
             pair ([k], [other_root]) into the priority queue.
           *)
          for n = 0 to k - 1 do
            if not roots.(n) then ()
            else begin
                let dist =
                  Distance.max distances.(min i n).(max i n) distances.(min j n).(max j n) in
                assert (distances.(n).(k) = Distance.Regular (-1));
                distances.(n).(k) <- dist;
                match dist with
                  | Distance.Infinity -> ()
                  | Distance.Regular dist ->
                     queue := PrioQueue.insert !queue dist (k, n)
              end
          done;
        end;
    done;
    (* Complexity analysis: the priority queue is large, of the order
       of O(N^2) entries during the traversal -- at a maintenance cost
       of O(N^2 log N) with our implementation.

       Each root-pair in the queue requires O(N) work to update the
       distance matrix and the priority queue.

       Naively this gives a O(N^3) bound, but in fact we encounter
       much fewer than N^2 root-pairs: each time we see a minimal
       root-pair, we create a new node, and we know that we create
       a most N nodes. So among the O(N^2) elements of the queue,
       only N require O(N) work, giving a total complexity of O(N^2 log N).
     *)
    List.concat (List.init !next_slot (fun i -> if roots.(i) then [nodes.(i)] else []))
  in
  List.concat (List.map cluster classes)


(*      ---4---
        |     |
  ---1---     ---2---
  A     B     C     D
*)
let test1 =
  let distances = [
      (('A', 'B'), 1);
      (('A', 'C'), 2);
      (('A', 'D'), 3);
      (('B', 'C'), 3);
      (('B', 'D'), 4);
      (('C', 'D'), 2);
    ] |> List.to_seq |> Hashtbl.of_seq
  in
  assert (
    (hierarchical_clustering_fast distances [['A'; 'B'; 'C'; 'D']])
    (* note: the order of A/B and C/D classes in the toplevel node
       is irrelevant, and an implementation that would swap them
       would also be correct *)
    = [Node(4, Node (2, Leaf 'C', Leaf 'D'), Node (1, Leaf 'A', Leaf 'B'))])

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
  let surapprox_leaves = List.map (List.map (fun x -> Leaf x)) surapprox in
  let dendrogram_list = hierarchical_clustering hdistance_matrix surapprox_leaves in
  debug "hierarchical_clustering done";
  let dendrogram_list_fast = hierarchical_clustering_fast hdistance_matrix surapprox in
  debug "hierarchical_clustering_fast done";
  ignore (dendrogram_list, dendrogram_list_fast);
  (* Note: despite my efforts, the clustering results are different between the slow
     and the fast algorithm. I don't know if there is a bug remaining in the fast
     algorithm. There are various ways in which two correct implementations could give
     distinct results:

     - the order of the two childrean of each node is irrelevant
     - when there exist several root pairs with the same minimal distance,
       we do not specify which one should be picked, and the choice influences
       the shape of the final result (picking a pair may make some other pairings
       impossible)

     In particular, if all nodes are at the same distance to each other,
     many different result trees are possible.
  *)
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
