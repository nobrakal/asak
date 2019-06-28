(* This file is part of asak.
 *
 * Copyright (C) 2019 Alexandre Moine.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

val hash_lambda :
  bool (* Do we sort hash lists ? *)
  -> int (* weight threshold, exprimed in percent compared to the weight of the main AST *)
  -> Lambda.lambda (* The lambda expression *)
  -> (int*string) * (int*string) list (* The main hash + hash of sub-AST over the threshold *)

(* Inline all possible expressions *)
val inline_all : Lambda.lambda -> Lambda.lambda
