let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let analysis database (name,hash) =
  match Asak.Clustering.HMap.find_opt hash database with
  | None -> ()
  | Some xs ->
     Printf.printf "%s has the same hash than:\n" name;
     List.iter (Printf.printf "* %s\n") xs;
     print_endline ""

let get_typedtree load f =
  let cmt = Cmt_format.read_cmt f in
  match cmt.cmt_annots with
  | Implementation structure ->
     Load_path.init (load @ cmt.cmt_loadpath);
     let map =
       { Tast_mapper.default
       with env = fun _ env -> Envaux.env_of_only_summary env } in
     let structure = map.structure map structure in
     cmt.cmt_modname,structure
  | _ -> failwith "not a structure"

let build_from merlin =
  let full_file = load_file merlin in
  let lines = String.split_on_char '\n' full_file in
  let builds = List.filter (fun x -> String.length x > 0 && String.get x 0 = 'B') lines in
  List.map (fun x -> String.sub x 2 (String.length x - 2)) builds

let prefix_of merlin =
  try
    let ind = String.rindex merlin '/' + 1 in
    Some (String.sub merlin 0 ind)
  with Not_found -> None

let add_prefix prefix xs =
  match prefix with
  | None -> xs
  | Some prefix ->
     let aux x =
       if String.get x 0 = '/'
       then x
       else prefix ^ x in
     List.map aux xs

let main database merlin typedtree =
  print_endline "usage: mis.exe db.asak path/to/.merlin tree.cmt";
  Compmisc.init_path true;
  let database : string list Asak.Clustering.HMap.t =
    Marshal.from_channel (open_in_bin database) in
  let load = add_prefix (prefix_of merlin) (build_from merlin) in
  let name,typedtree = get_typedtree load typedtree in
  let lambdas = Asak.Parse_structure.read_structure name typedtree in
  let lambdas =
    Asak.Lambda_hash.map_snd
      (fun x -> Asak.Lambda_normalization.(normalize_local_variables (inline_all x)))
      lambdas in
  let hash_list =
    Asak.Lambda_hash.(hash_all {should_sort=false; hash_var=true} 0 lambdas) in
  let main_hash_list = List.map (fun (x,(h,_)) -> x,h) hash_list in
  List.iter (analysis database) main_hash_list

let () = main Sys.argv.(1) Sys.argv.(2) Sys.argv.(3)
