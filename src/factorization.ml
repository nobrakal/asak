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

let filter_rev_map_print pred =
  List.fold_left
    (fun acc x ->
      let name, x = pred x in
      either
        (fun x -> x :: acc)
        (fun s -> print_endline ("[" ^ name ^ "] Warning: " ^ s); acc)
        (run x))
    []

let parse_all_implementations hard_weight files_list =
  let pred (must_open,lib,filename) =
    lib,
    parsetree_of_string (load_file filename)
    >>=
      (if must_open
      then type_with_init ~to_open:lib
      else type_with_init ?to_open:None)
    >>= fun r ->
    ret @@
      Lambda_utils.hash_all {should_sort=false} hard_weight @@
        read_structure lib r
  in List.concat @@ filter_rev_map_print pred files_list

let search hard_weight files_list =
  let all_hashs = parse_all_implementations hard_weight files_list in
  Clustering.cluster all_hashs
