(* This file is part of asak.
 *
 * Copyright (C) 2019 Alexandre Moine.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type 'a partition =
  { bad_type : 'a list ;
    clusters : 'a list Wtree.wtree list;
  }

val partition :
  int (* Percent of subtrees to keep *)
  -> string (* The searched function *)
  -> string (* A reference implementation, used only for typing *)
  -> ('a * string) list (* List of codes maybe containing the function, each identified by a key *)
  -> 'a partition
