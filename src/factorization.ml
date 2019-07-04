(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Monad_error

open Parse_structure
open ErrS

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let find_name x =
  match x with
  | Lambda.Llet (_,_,n,_,_) -> Some (Ident.name n)
  | Lambda.Lletrec (xs,_) -> (* There is only one definition *)
     Some (Ident.name @@ fst @@ List.hd xs)
  | _ -> None

let add_name pref x =
  match find_name x with
  | Some x -> Some (pref ^ "/" ^ x)
  | None -> None

let filter_rev_map_print pred =
  List.fold_left
    (fun acc x ->
      let name, x = pred x in
      either
        (fun s -> print_endline ("[" ^ name ^ "] Warning: " ^ s); acc)
        (fun x -> x :: acc)
        (run x))
    []

let hash_all hard_weight t =
  let open ErrS in
  let threshold = Lambda_utils.Hard hard_weight in
  let hash x =
    let (x,xs) = Lambda_utils.hash_lambda false threshold x in
    x::xs in
  let hash_with_name x =
    err_of_option "" (add_name t x)
    >>= fun t -> ret (t, hash x)
  in filter_rev_map
       (fun x -> run (hash_with_name x))

let rec last = function
  | [] -> failwith "last"
  | [x] -> x
  | _::xs -> last xs

let parse_all_implementations hard_weight files_list =
  let pred (must_open,lib,filename) =
    lib,
    let pretty_filename = last @@ String.split_on_char '/' filename in
    parsetree_of_string (load_file filename)
    >>=
      (if must_open
      then type_with_init ~to_open:lib
      else type_with_init ?to_open:None)
    >>= fun r ->
    ret @@
      hash_all hard_weight (lib ^ "." ^ pretty_filename) @@
        rev_lambdas_of_lst lib r
  in List.concat @@ filter_rev_map_print pred files_list

let search hard_weight files_list =
  let all_hashs = parse_all_implementations hard_weight files_list in
  Clustering.cluster all_hashs
