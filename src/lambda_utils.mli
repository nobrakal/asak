(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type threshold =
  | Percent of int (** A percentage threshold, a number between 0 and 100. *)
  | Hard of int (** A hard threshold. *)

type config =
  { should_sort : bool; (** If we sort all lists of hash. *)
    hash_var : bool; (** If we hash names in the AST. *)
  }

(** Hash a lambda expression. Usage: [hash_lambda config threshold expr].
    Two lambda expressions "of the same shape" will share the same hash.

    @param config A configuration for the function.

    @param threshold Used to decide which sub-AST hash is kept.

    @param expr The expression.

     @return A tuple with the main hash, and a list of hashes of sub-ASTs.
All hashes are given with the weight of their AST.
*)
val hash_lambda :
  config ->
  threshold ->
  Lambda.lambda -> (int * string) * (int * string) list

(** Using a hard threshold, hash a list of lambda expressions from  {! Parse_structure.read_structure }. *)
val hash_all :
  config ->
  int ->
  ('a * Ident.t * Lambda.lambda) list ->
  ('a * ((int * string) * (int * string) list)) list

(** Escape hashs. *)
val escape_hash_list :
  ('a * ((int * string) * (int * string) list)) list
  -> ('a * ((int * string) * (int * string) list)) list

(** Inline all possible (ie. without side effect) expressions in a lambda expression. *)
val inline_all : Lambda.lambda -> Lambda.lambda
