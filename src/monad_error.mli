(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type ('a,'b) either = Left of 'a | Right of 'b

val either : ('a -> 'c) -> ('b -> 'c) -> ('a,'b) either -> 'c

module type T = sig
  type e
end

module Make (T : T) : sig
  type 'a t

  val fail : T.e -> 'a t
  val ret : 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val run : 'a t -> (T.e,'a) either
  val to_err : (T.e,'a) either -> 'a t
  val err_of_option : T.e -> 'a option -> 'a t
end

module ErrS : sig
  type 'a t

  val fail : string -> 'a t
  val ret : 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val run : 'a t -> (string,'a) either
  val to_err : (string,'a) either -> 'a t
  val err_of_option : string -> 'a option -> 'a t
end

val filter_rev_map : ('a -> ('b,'c) either) -> 'a list -> 'c list
