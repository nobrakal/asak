(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Wtree

(** Given a list of AST hashes (identified by a key), perform a kind of complete-linkage
    clustering using a custom distance based on symmetric difference.

    The result is a list of "guaranteed different" identifier trees, where
    two identifier are in the same tree if they share at least one hash, and in
    the same leaf if they share extacly the same hash list.
 *)
val cluster : ('a * (int * string) list) list -> ('a list) wtree list
