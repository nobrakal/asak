(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

let rec last = function
  | [] -> failwith "last"
  | [x] -> x
  | _::xs -> last xs
