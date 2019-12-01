open Asak.Wtree

let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let remove_version x =
  try
    let ind = String.index x '.' in
    let indtd = String.index x ':' in
    String.sub x 0 ind ^ String.sub x indtd (String.length x - indtd )
  with | Not_found -> x

let remove_version_all =
  List.rev_map
    (fold_tree
       (fun a b c -> Node (a,b,c))
       (fun xs -> Leaf (List.sort_uniq compare (List.rev_map remove_version xs))))

let sum xs = List.fold_left ( + ) 0 xs

module IntMap = Map.Make(struct type t = int let compare = compare end)

let update_plus_one = function
  | None -> Some 1
  | Some x -> Some (x+1)

let size_of_class = fold_tree (fun _ -> ( + ) ) List.length

let take_first limit xs =
  let rec aux i = function
  | [] -> []
  | x::xs ->
    if i < limit
    then x::(aux (i+1) xs)
    else []
  in aux 0 xs

let compare_m_fst (x,_) (y,_) = - compare x y

let flatten_tree t = fold_tree (fun _ -> List.rev_append) (fun x -> x) t

let print_first toinspect thres =
  let rec aux i xs =
    if i < toinspect
    then
      match xs with
      | [] -> failwith "print_first_10"
      | (s,x)::xs ->
         Printf.printf "Class n°%d of size %d\n" i s;
         List.iter print_endline
           (take_first thres (shuffle (flatten_tree x)));
         print_endline "";
         aux (i+1) xs
  in aux 0

let index_if_exist s c1 c2 =
  try
    String.index s c1
  with
  | Not_found -> String.index s c2

module SSet =
  Set.Make (
      struct
        type t = string
        let compare = compare
      end )

let get_prefix s =
  let open String in
  sub s 0 (index_if_exist s '-' '.')

let get_prefixes t =
  fold_tree
    (fun _ -> SSet.union)
    (List.fold_left (fun acc x -> SSet.add (get_prefix x) acc) SSet.empty) t

let refine_classes l xs =
  let is_more_than_l_package t =
    SSet.cardinal (get_prefixes t) > l in
  List.filter is_more_than_l_package xs

let export_csv classes_with_size = function
  | None -> ()
  | Some csvfile ->
     let assoc_map =
       List.fold_left (fun acc (x,_) -> IntMap.update x update_plus_one acc) IntMap.empty classes_with_size in
     let plot =
       IntMap.fold (fun k v acc -> acc ^ string_of_int k ^ "," ^ string_of_int v ^ "\n") assoc_map "" in
     let chan = open_out csvfile in
     output_string chan plot;
     close_out chan

let print_infos toinspect thres (classes : string list wtree list) csvfile =
  let list_of_lengths = List.rev_map size_of_class classes in
  let nb_defs = sum list_of_lengths in
  Printf.printf "Number of let-definitions: %d\n" nb_defs;
  let nb_classes = List.length classes in
  Printf.printf "Number of classes: %d\n" nb_classes;
  let real_class =
    List.filter (function | Leaf [] | Leaf [_] -> false | _ -> true)
      classes in
  let nb_real_class = List.length real_class in
  Printf.printf "Number of classes with (strictly) more than one element: %d\n"
    nb_real_class;
  let classes_with_size = List.sort compare_m_fst @@ List.map (fun x -> size_of_class x, x) real_class in
  let maxe = fst @@ List.hd classes_with_size in
  Printf.printf "The biggest class is of size: %d\n" maxe;
  let median = fst @@ List.nth classes_with_size (nb_real_class / 2) in
  Printf.printf "Median of size of classes with more than one element: %d\n" median;
  print_first toinspect thres classes_with_size;
  export_csv classes_with_size csvfile

let map_add_sufix x s =
  Option.map (fun x -> x ^ s) x

let main filename toinspect thres csvfile =
  Random.init 42;
  let chan = open_in_bin filename in
  let all_cluster : (string list) wtree list = Marshal.from_channel chan in
  print_endline "When considering different versions of the same package:";
  print_infos toinspect thres all_cluster (map_add_sufix csvfile "all.csv");
  print_endline "When considering only one time a given function of a given package:";
  print_infos toinspect thres (remove_version_all all_cluster) (map_add_sufix csvfile "only.csv");
  print_endline "After removing classes with less than 2 packages :";
  print_infos toinspect thres (refine_classes 3 all_cluster) (map_add_sufix csvfile "refinement.csv")

let get_opt arr i =
  try Some (Array.get arr i) with
  | Invalid_argument _ -> None

let () =
  main
    Sys.argv.(1)
    (int_of_string Sys.argv.(2))
    (int_of_string Sys.argv.(3))
    (get_opt Sys.argv 4)
