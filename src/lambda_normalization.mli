(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Inline all possible (ie. without side effect) expressions in a lambda expression. *)
val inline_all : Lambda.lambda -> Lambda.lambda
