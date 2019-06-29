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
  let hash x =
    let (x,xs) = Lambda_utils.hash_lambda false threshold x in
    x::xs in
  let rec aux = function
    | Lambda.Lsequence (x,u) -> (t, hash x) :: aux u
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
  Clustering.cluster all_hashs
