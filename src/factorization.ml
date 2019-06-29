(* This file is part of asak.
 *
 * Copyright (C) 2019 Alexandre Moine.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Monad_error
open Err

open Parse_fragment

let split_sequence_with hard_weight t =
  let threshold = Lambda_utils.Hard hard_weight in
  let hash x = snd @@ fst @@ Lambda_utils.hash_lambda false threshold x in
  let rec aux = function
    | Lambda.Lsequence (x,u) -> print_endline (hash x);  (t, hash x) :: aux u
    | x -> [t, hash x]
  in aux

let parse_all_implementations hard_weight lst =
  let pred (t,save) =
    parsetree_of_string save
    >>= type_with_init
    >>= fun r ->
    let r = split_sequence_with hard_weight t @@ lambda_of_typedtree r in
    ret r
  in List.concat @@ filter_rev_map pred lst

let search hard_weight str_list =
  let all_hashs = parse_all_implementations hard_weight str_list in
  let hash_set = Hashtbl.create 100 in
  List.iter
    (fun (t,x) ->
      match Hashtbl.find_opt hash_set x with
      | None -> Hashtbl.add hash_set x [t]
      | Some ts ->
         Hashtbl.remove hash_set x;
         Hashtbl.add hash_set x (t::ts)
    ) all_hashs;
  Hashtbl.fold
    (fun _ xs acc ->
      if List.length xs > 1
      then xs::acc
      else acc
    ) hash_set []
