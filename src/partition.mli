(* This file is part of asak.
 *
 * Copyright (C) 2019 Alexandre Moine.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Type used for presenting a partition. *)
type 'a partition =
  { bad_type : 'a list ;
    (** Contains keys identifying codes that either don't implement the searched function,
        or implement it with a bad type. *)
    clusters : 'a list Wtree.wtree list;
    (** Main result.
        Each class is composed of a tree representing a hierarchical clustering of "similar codes".
        Each leaf is a list of keys identifying "totally similar" codes.
*)
  }

(** Partition OCaml codes
    @param int Weight threshold required to keep sub-AST. It is exprimed
as percent (a number between 0 and 100) in term of the weight of the whole AST.

- A value of 0 means "keep all sub-ASTs"
- A value of 100 means "don't keep any sub-AST"

    @param string1 The name of the searched function
    @param string2 A reference implementation, used only for typing
    @param list List of valid OCaml codes, using only the standard library, that may
contains the searched function.

         @return The partition
*)
val partition :
  int
  -> string
  -> string
  -> ('a * string) list
  -> 'a partition
