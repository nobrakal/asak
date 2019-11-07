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
  val lt : t -> t -> bool
  val max : t -> t -> t
  val min : t -> t -> t

end

module Hash :
  sig
    type t = Lambda_hash.fingerprint
    val compare : t -> t -> int
  end

module HMap : Map.S with type key = Hash.t

(** Compute the symmetric difference of two {e sorted} lists.
    Return None if the intersection was empty. *)
val symmetric_difference : 'a list -> 'a list -> 'a list option

(** Return the sum of the weight of the symmetric difference
    of the two arguments (or [Infinity] if the intersection was empty *)
val semimetric :
  (int * 'a) list -> (int * 'a) list -> Distance.t

(** Compute recursively the dissimilarity between two clusters:

- If there is two {!Wtree.Leaf}, use the sum of the weight of the symmetric difference
  of their hash lists (or [Infinity] if the intersection was empty).
- If there is a {!Wtree.Node}, use the {e maximum} of the dissimilarities between the sub-trees
  and the other tree.

This is not a mathematically valid distance, but only a semimetric.

The first argument must be an equivalent of {!semimetric}.
 *)
val dist :
  ('a -> 'a -> Distance.t) ->
  'a Wtree.wtree -> 'a Wtree.wtree -> Distance.t

(** Create initial cluster, grouping labels by fingerprint *)
val initial_cluster : ('a * Lambda_hash.fingerprint) list -> 'a list HMap.t

(** Given a list of AST hashes (identified by a key), perform a kind of complete-linkage
    clustering using {!semimetric}.

    @param cores The number of cores that the function can use
    (by default, try to detect how many the computer has).

    @param filter_small_trees If specified, will remove hashes
    that do not have at least this weight.

    @return A list of trees, where
    two keys are in the same tree if they share at least one hash and in
    the same leaf if they share exactly the same hash list.

    The list is sorted with biggest trees first.
 *)
val cluster :
  ?cores:int
  -> ?filter_small_trees:int
  -> ('a * Lambda_hash.hash) list -> ('a list) wtree list

(** Print recursively a cluster given a printer for the labels. *)
val print_cluster : ('a -> string) -> ('a list) wtree list -> unit
