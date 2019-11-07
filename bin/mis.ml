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
     Load_path.init (load :: cmt.cmt_loadpath);
     let map =
       { Tast_mapper.default
       with env = fun _ env -> Envaux.env_of_only_summary env } in
     let structure = map.structure map structure in
     cmt.cmt_modname,structure
  | _ -> failwith "not a structure"

let main database load typedtree =
  Compmisc.init_path true;
  let database : string list Asak.Clustering.HMap.t =
    Marshal.from_channel (open_in_bin database) in
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
