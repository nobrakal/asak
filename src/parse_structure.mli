(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Monad_error

val parsetree_of_string : string -> Parsetree.structure ErrS.t

val init_env : ?to_open:string -> unit -> Env.t
val type_with_init : ?to_open:string -> Parsetree.structure -> Typedtree.structure ErrS.t

val rev_lambdas_of_lst : string -> Typedtree.structure -> Lambda.lambda list
