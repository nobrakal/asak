(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Type used to represent a partition. *)
type 'a partition =
  { bad_type : 'a list ;
    (** Keys identifying codes that either don't implement the searched function,
        or implement it with a bad type. *)
    clusters : ('a * string) list Wtree.wtree list
    (** Main result.
        Each class is composed of a tree representing a hierarchical clustering of "similar codes".
        Each leaf is a list of keys identifying "totally similar" codes, plus the considered
        code.
*)
  }

(** Partitioning OCaml codes. Usage: [create threshold name sol list].

    @param threshold Weight threshold required to keep a sub-AST. It is a percentage
(a number between 0 and 100) of the weight of the whole AST.

- A value of 0 means "keep all sub-ASTs"
- A value of 100 means "don't keep any sub-AST"

    @param name The name of the searched function.
    @param sol A reference implementation, used only for typing.
    @param list List of valid OCaml codes, using only installed modules,
    that may contains the searched function.

    @return The partition.
*)
val create :
  int
  -> string
  -> string
  -> ('a * string) list
  -> 'a partition
