(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Monad_error

(** If string is a valid OCaml code, return its parsetree. *)
val parsetree_of_string : string -> Parsetree.structure ErrS.t

(** Initial typing envrionement, maybe with an other openened library. *)
val init_env : ?to_open:string -> unit -> Env.t

(** Typecheck a parsetree with the initial environement. *)
val type_with_init : ?to_open:string -> Parsetree.structure -> Typedtree.structure ErrS.t

(** Check if a [value_binding] is a variable with the given name. *)
val has_name : string -> Typedtree.value_binding -> bool

(** Extract a specific let binding in a typedtree,
    and transform it in a lambda expression. *)
val get_specific_lambda_of_typedtree : string -> Typedtree.structure -> Lambda.lambda ErrS.t

(** Find a specific toplevel let-definition. *)
val find_let_in_parsetree_items :
  string -> Parsetree.structure -> Parsetree.structure_item option

(** Return all let bindings of a typedtree converted in lambda expressions. *)
val rev_lambdas_of_lst : string -> Typedtree.structure -> Lambda.lambda list
