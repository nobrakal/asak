(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Partition OCaml codes. Usage: [search threshold list]
    @param threshold Weight threshold required to keep a sub-AST.

    @param list List of valid OCaml files, where each element is of the form
    [(should_open, lib_name, file_path)] where

- should_open is a boolean, indicating if the tool should open the library when
type-checking (this is useful when running on a file that is part of a library).
- lib_name is the name of the library of the file.
- file_path is the path of the searched file.

     @return The result is a clustering of toplevel definitions.
     Two toplevel definitions are in the same tree if they share a substantial sub-AST,
     and they will be in the same leaf if they have exactly the same hash.
*)
val search : int -> (bool * string * string) list -> string list Wtree.wtree list
