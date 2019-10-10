open Asak.Wtree

let get_prefix x =
  try
    let ind = String.index x '.' in
    String.(sub x 0 ind)
  with | Not_found -> x

let sum xs = List.fold_left ( + ) 0 xs

let rec last = function
  | [] -> failwith "last"
  | [x] -> x
  | _::xs -> last xs

module IntMap = Map.Make(struct type t = int let compare = compare end)

let update_plus_one = function
  | None -> Some 1
  | Some x -> Some (x+1)

let size_of_class = fold_tree (fun _ -> ( + ) ) List.length

let print_infos (classes : string list wtree list) csvfile =
  let list_of_lengths = List.rev_map size_of_class classes in
  let nb_defs = sum list_of_lengths in
  Printf.printf "Number of let-definitions: %d\n" nb_defs;
  let nb_classes = List.length classes in
  Printf.printf "Number of classes: %d\n" nb_classes;
  let real_class =
    List.filter (function | Leaf _ -> false | _ -> true)
      classes in
  let nb_real_class = List.length real_class in
  Printf.printf "Number of classes with (strictly) more than one element: %d\n"
    nb_real_class;
  let size_of_classes = List.sort compare @@ List.map size_of_class real_class in
  let maxe = last size_of_classes in
  Printf.printf "The biggest class is of size: %d\n" maxe;
  let median = List.nth size_of_classes (nb_real_class / 2) in
  Printf.printf "Median of size of classes with more than one element: %d\n" median;
  let assoc_map = List.fold_left (fun acc x -> IntMap.update x update_plus_one acc) IntMap.empty size_of_classes in
  let plot = IntMap.fold (fun k v acc -> acc ^ string_of_int k ^ "," ^ string_of_int v ^ "\n") assoc_map "" in
  let chan = open_out csvfile in
  output_string chan plot;
  close_out chan

let print_infos_without_versions (before_clustering : string list wtree list) csvfile =
  let class_with_only_lib_name =
    List.rev_map
      (fold_tree
         (fun a b c -> Node (a,b,c))
         (fun xs -> Leaf (List.sort_uniq compare @@ List.rev_map get_prefix xs)))
      before_clustering in
  print_infos class_with_only_lib_name csvfile

let main filename csvfile =
  let chan = open_in_bin filename in
  let all_cluster : (string list) wtree list = Marshal.from_channel chan in
  print_endline "When considering different versions of the same package:";
  print_infos all_cluster (csvfile ^ "all.csv");
  print_endline "When considering only one time a given definition for a given package:";
  print_infos_without_versions all_cluster (csvfile ^ "uniq.csv")

let () = main Sys.argv.(1) Sys.argv.(2)
