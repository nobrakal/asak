(* This file is part of asak.
 *
 * Copyright (C) 2019 Alexandre Moine.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Wtree

val cluster : ('a, (int * string) list) Hashtbl.t -> ('a list) wtree list
