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

let hash_and_compare name hash str1 str2 () =
  let lambda1 = get_last_lambda_of_str str1 in
  let lambda2 = get_last_lambda_of_str str2 in
  let open Alcotest in
  check testable_hash name (hash lambda1) (hash lambda2)

let hash_strict =
  Lambda_utils.(hash_lambda {should_sort=false;hash_var=true} threshold)

let tests_same_hash =
  [("inline"     , "let f x = let a x = x in a x", "let f x = x"      );
   ("alpha-conv1", "let f a b = a + b"           , "let f x y = x + y");
   ("alpha-conv2", "let f a = let b = a in a + b", "let f a = let x = a in a + x");
  ]

let same_hash =
  let run_test (name, str1, str2) =
    Alcotest.test_case name `Quick @@
      hash_and_compare "same_hash" hash_strict str1 str2 in
  List.map run_test tests_same_hash

let () =
  let open Alcotest in
  run "asak" [
      "same hash", same_hash;
    ]
