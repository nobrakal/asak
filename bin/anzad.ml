open Location

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let extract_dir_from_file f =
  try
    let ind = String.rindex f '/' in
    String.sub f 0 ind, String.sub f (ind + 1) (String.length f - ind -1)
  with
  | Not_found -> ".",f

let rec keep_only i = function
  | [] -> []
  | x::xs ->
     if i <= 0
     then []
     else x::(keep_only (i-1) xs)

let analysis is_for_emacs limit database ((name,({loc_start;loc_end;_} as loc)),hash) =
  let open Lexing in
  match Asak.Clustering.HMap.find_opt hash database with
  | None -> ()
  | Some xs ->
     match List.filter (fun x -> x <> name) xs with
     | [] -> ()
     | xs ->
        if is_for_emacs
        then
          begin
            Printf.printf "%d;%d;* %s#" loc_start.pos_cnum loc_end.pos_cnum
              (String.concat "\n* " xs)
          end
        else
          begin
            Printf.printf "%s in " name;
            Location.print_loc Format.std_formatter loc;
            Format.pp_print_flush Format.std_formatter ();
            Printf.printf " has the same hash than:\n";
            List.iter (Printf.printf "* %s\n") (keep_only limit xs);
            print_endline ""
          end

let load_path_init xs =
#if OCAML_VERSION >= (4, 08, 0)
  Load_path.init xs
#else
  Config.load_path := xs @ !Config.load_path
#endif

let get_typedtree load f =
  Asak.Parse_structure.init_path ();
  let cmt = Cmt_format.read_cmt f in
  match cmt.cmt_annots with
  | Implementation structure ->
     load_path_init (load @ cmt.cmt_loadpath);
     let map =
       { Tast_mapper.default
       with env = fun _ env -> Envaux.env_of_only_summary env } in
     let structure = map.structure map structure in
     cmt.cmt_modname,structure
  | _ -> failwith "not a structure"

(** Get all lines starting by 'B' in a .merlin *)
let build_from merlin =
  let full_file = load_file merlin in
  let lines = String.split_on_char '\n' full_file in
  let builds = List.filter (fun x -> String.length x > 0 && String.get x 0 = 'B') lines in
  List.map (fun x -> String.sub x 2 (String.length x - 2)) builds

let add_prefix prefix xs =
  let aux x =
    if String.get x 0 = '/'
    then x
    else prefix ^ x in
  List.map aux xs

(** Given a file = "source.ml", will search for a file with a "Source.cmt" suffix
    in the given path *)
let find_cmt_in_paths file paths =
  let file_prefix = String.(sub file 0 (rindex file '.')) in
  let mod_name =
    let open String in
    let f = make 1 (Char.uppercase_ascii (get file_prefix 0)) in
    f ^ (sub file_prefix 1 (length file_prefix - 1))
  in
  let add_cmt x = x ^ ".cmt" in
  let possibilities = [add_cmt file_prefix; add_cmt mod_name] in
  let is_suffix x s =
    try
      s = String.(sub x (length x - length s) (length s))
    with
    | Invalid_argument _ -> false in
  let aux dir =
    let files = Sys.readdir dir in
    List.find (fun x -> List.exists (is_suffix x) possibilities) (Array.to_list files) in
  let res =
    List.fold_left
      (fun acc x ->
        try Some (x ^ "/" ^ aux x)
        with Not_found -> acc
      ) None paths in
  match res with
  | None -> failwith "could not find the corresponding cmt"
  | Some x -> x

let update_db =
  let update_binding db ((x,_),h) =
    let xs =
      try Asak.Clustering.HMap.find h db
      with Not_found -> [] in
    Asak.Clustering.HMap.add h (x::xs) db in
  List.fold_left update_binding

let hash_file full_file =
  let dir,file = extract_dir_from_file full_file in
  let merlin = dir ^ "/.merlin" in
  let load =  add_prefix (dir ^ "/") (build_from merlin) in
  let typedtree = find_cmt_in_paths file load in
  let name,typedtree = get_typedtree load typedtree in
  let lambdas = Asak.Parse_structure.read_structure_with_loc name typedtree in
  let lambdas =
    Asak.Lambda_hash.map_snd
      (fun x -> Asak.Lambda_normalization.(normalize_local_variables (inline_all x)))
      lambdas in
  let hash_list =
    Asak.Lambda_hash.(hash_all {should_sort=false; hash_var=true} 0 lambdas) in
  List.map (fun (x,(h,_)) -> x,h) hash_list

let main is_for_emacs limit database files =
  let main_hash_list = List.concat (List.map hash_file files) in
  let database : string list Asak.Clustering.HMap.t =
    match database with
    | None -> Asak.Clustering.HMap.empty
    | Some database ->
       Marshal.from_channel (open_in_bin database) in
  let database = update_db database main_hash_list in
  List.iter (analysis is_for_emacs limit database) main_hash_list

open Cmdliner

let limit =
  let doc = "Limit of size of printed classes." in
  Arg.(value & opt int 10 & info ["l";"limit"] ~doc)

let database =
  let doc = "The asak database." in
  Arg.(value & opt (some string) None & info ["d";"database"] ~doc)

let file =
  let doc = "The path to a file.\n NB: it must be located in a directory which contains a .merlin" in
  Arg.(non_empty & pos_all string [] & info [] ~doc)

let machine =
  let doc = "If the output should be machine-readable." in
  Arg.(value & flag & info ["m";"machine"] ~doc)

let info =
  let doc = "Run Asak on a file." in
  let man = [
      `S Manpage.s_bugs;
      `P "Report bugs to: https://github.com/nobrakal/asak/issues." ]
  in
  Term.info "anzad" ~doc ~exits:Term.default_exits ~man

let main_t = Term.(const main $ machine $ limit $ database $ file)

let () =
  Term.exit @@ Term.eval (main_t, info)
