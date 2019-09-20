(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Asak

let threshold = Lambda_utils.Hard 0

let rec last x xs =
  match xs with
  | [] -> x
  | x::xs -> last x xs

let get_last_lambda_of_str str =
  match Parse_structure.read_string str with
  | [] -> failwith "empty"
  | x::xs ->
     let (_,_,lst) = last x xs in
     lst

let testable_hash =
  let open Alcotest in
  let hash = pair int string in
  pair hash (slist hash compare) 
     
let hash_and_compare name hash str1 str2 =
  let lambda1 = get_last_lambda_of_str str1 in
  let lambda2 = get_last_lambda_of_str str2 in
  let open Alcotest in
  check testable_hash name (hash lambda1) (hash lambda2)

let hash_strict =
  Lambda_utils.(hash_lambda {should_sort=false;hash_var=true} threshold)

let same_hash = "same hash"

let test_inline1 () =
  let str1 = "let f x = let a x = x in a x" in
  let str2 = "let f x = x" in
  hash_and_compare same_hash hash_strict str1 str2

let () =
  let open Alcotest in
  run "asak" [
      "inline_all", [
        test_case "test_inline1" `Quick test_inline1;
      ];
    ]
