(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Wtree

module Distance : sig

  type t = Regular of int | Infinity

  val compare : t -> t -> int
  val ( < ) : t -> t -> bool
  val max : t -> t -> t
  val min : t -> t -> t

end

(** Compute the symmetric difference of two {e sorted} lists.
    Return also if the intersection was not empty. *)
val symmetric_difference : 'a list -> 'a list -> bool * 'a list

(** Compute recursively the distance between two clusters:

- If there is two {!Wtree.Leaf}, use the sum of the weight of the symmetric difference
  of their hash lists (or [Infinity] if the intersection was empty).
- If there is a {!Wtree.Node}, use the {e maximum} of the distances between the sub-trees
  and the other tree.
 *)
val dist :
  ((int * string) list * 'b) wtree -> ((int * string) list * 'b) wtree
  -> Distance.t

(** Given a list of AST hashes (identified by a key), perform a kind of complete-linkage
    clustering using {!dist}.

    @return A list of trees, where
    two keys are in the same tree if they share at least one hash, and in
    the same leaf if they share exactly the same hash list.

    The list is sorted with biggest trees first.
 *)
val cluster : ('a * (int * string) list) list -> ('a list) wtree list
