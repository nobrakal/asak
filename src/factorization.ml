(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Monad_error
open Utils

open Parse_structure
open ErrS

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let find_name = function
  | Lambda.Llet (_,_,n,_,_) -> Ident.name n
  | _ -> "noname"

let add_name pref x = pref ^ "/" ^ find_name x

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
  let threshold = Lambda_utils.Hard hard_weight in
  let hash x =
    let (x,xs) = Lambda_utils.hash_lambda false threshold x in
    x::xs in
  List.map (fun x -> (add_name t x, hash x) )

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
