(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type threshold =
  | Percent of int (** A percentage threshold, a number between 0 and 100. *)
  | Hard of int (** A hard threshold. *)

(** Hash a lambda expression. Usage: [hash_lambda will_sort threshold expr].
    Two lambda expressions "of the same shape" will share the same hash.

    @param will_sort If true, then all lists of hash will be sorted.
This is useful to identify more codes, but can leads to some false-positive.
     @param threshold Used to decide which sub-AST's hash is kept.
     @param expr The expression.

     @return A tuple with the main hash, and a list of hashes of sub-ASTs.
All hashes are given with the weight of their AST.
*)
val hash_lambda :
  bool
  -> threshold
  -> Lambda.lambda
  -> (int*string) * (int*string) list

(** Inline all possible (ie. without side effect) expressions in a lambda expression. *)
val inline_all : Lambda.lambda -> Lambda.lambda

(** When two applications are composed, make only one. *)
val apply_full : Lambda.lambda -> Lambda.lambda
