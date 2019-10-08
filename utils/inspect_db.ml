open Asak.Wtree

let rec to_list_append xs t =
  match t with
  | Leaf x -> x::xs
  | Node (_,l,r) ->
     let r = to_list_append xs r in
     to_list_append r l

let get_prefix x =
  try
    let ind = String.index x '.' in
    String.(sub x 0 ind)    
  with | Not_found -> x

let main filename =
  let chan = open_in_bin filename in
  let all_cluster : (string list) wtree list = Marshal.from_channel chan in
  let before_clustering =
    List.fold_left (to_list_append) [] all_cluster in
  let class_with_only_lib_name =
    List.rev_map
      (fun xs -> List.sort_uniq compare @@ List.rev_map get_prefix xs)
      before_clustering in
  let nb_classes = List.length class_with_only_lib_name in
  Printf.printf "Number of class: %d\n" nb_classes;
  let real_class =
    List.filter (function | [] | [_] -> false | _ -> true)
      class_with_only_lib_name in
  let nb_real_class = List.length real_class in
  Printf.printf "Number of class with more than one element: %d\n" nb_real_class;
  let size_of_class = List.map List.length real_class in
  let median = List.nth (List.sort compare size_of_class) (nb_real_class / 2) in
  Printf.printf "Median of size of class with more than one element: %d\n" median

  
let () = main Sys.argv.(1)
