open Asak.Wtree

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

let print_first toinspect =
  let rec aux i xs =
    if i < toinspect
    then
      match xs with
      | [] -> failwith "print_first_10"
      | (s,x)::xs ->
         Printf.printf "Class n°%d of size %d\n" i s;
         List.iter print_endline 
           (take_first 10 (fold_tree (fun _ x _ -> x) (fun x -> x) x));
         print_endline "";
         aux (i+1) xs
  in aux 0

let print_infos toinspect (classes : string list wtree list) csvfile =
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
  print_first toinspect classes_with_size;
  let assoc_map =
    List.fold_left (fun acc (x,_) -> IntMap.update x update_plus_one acc) IntMap.empty classes_with_size in
  let plot = IntMap.fold (fun k v acc -> acc ^ string_of_int k ^ "," ^ string_of_int v ^ "\n") assoc_map "" in
  let chan = open_out csvfile in
  output_string chan plot;
  close_out chan

let main filename toinspect csvfile =
  let chan = open_in_bin filename in
  let all_cluster : (string list) wtree list = Marshal.from_channel chan in
  print_endline "When considering different versions of the same package:";
  print_infos toinspect all_cluster (csvfile ^ "all.csv");
  print_endline "When considering only one time a given function of a given package:";
  print_infos toinspect (remove_version_all all_cluster) (csvfile ^ "only.csv")

let () = main Sys.argv.(1) (int_of_string Sys.argv.(2)) Sys.argv.(3)
