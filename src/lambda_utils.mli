(* This file is part of asak.
 *
 * Copyright (C) 2019 Alexandre Moine.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type threshold = Percent of int | Hard of int

(** Hash a lambda expression
@param bool If true, then all lists of hash will be sorted.
@param threshold Used to decide which sub-AST's hash is kept
@param lambda The expression

@return A tuple with the main hash, and a list of hashes of the sub-ASTs.
All hash are given with the weight of their AST.
*)
val hash_lambda :
  bool
  -> threshold
  -> Lambda.lambda
  -> (int*string) * (int*string) list

(** Inline all possible (ie. without side effects) expressions in a lambda expression *)
val inline_all : Lambda.lambda -> Lambda.lambda
