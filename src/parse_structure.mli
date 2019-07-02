(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Monad_error

val parsetree_of_string : string -> Parsetree.structure Err.t

val init_env : unit -> Env.t
val type_with_init : Parsetree.structure -> Typedtree.structure Err.t
val lambda_of_typedtree : Typedtree.structure -> Lambda.lambda
