let read threshold x =
  let open Asak.Lambda_utils in
  let chan = open_in x in
  let res =
    hash_all {should_sort=false;hash_var=true} threshold @@
      Marshal.from_channel chan in
  close_in chan;
  res

let get_args () =
  match Array.to_list Sys.argv with
  | _::nb::xs -> (int_of_string nb,xs)
  | _ -> failwith "Specify a threshold."

let () =
  let (threshold,xs) = get_args () in
  let read = read threshold in
  let all_hashs =
    List.fold_left
      (fun acc x -> read x @ acc) [] xs in
  let all_cluster = Asak.Clustering.cluster all_hashs in
  Asak.Clustering.print_cluster (fun x -> x) all_cluster
