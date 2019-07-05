(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Type for weighted trees. *)
type 'a wtree =
  | Node of (int * 'a wtree * 'a wtree)
  | Leaf of 'a

val fold_tree : (int -> 'b -> 'b -> 'b) -> ('a -> 'b) -> 'a wtree -> 'b

val size_of_tree : ('a -> int) -> 'a wtree -> int
