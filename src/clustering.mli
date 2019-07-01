(* This file is part of asak.
 *
 * Copyright (C) 2019 Alexandre Moine.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Wtree

(** Given a list of AST hashes (identified by a key), perform a complete-linkage clustering
    using a custom distance based on symmetric difference. *)
val cluster : ('a * (int * string) list) list -> ('a list) wtree list
