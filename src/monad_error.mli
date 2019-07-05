(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

val either : ('a -> 'c) -> ('b -> 'c) -> ('a,'b) result -> 'c

module type T = sig
  type e
end

module Make (T : T) : sig
  type 'a t

  val fail : T.e -> 'a t
  val ret : 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val run : 'a t -> ('a,T.e) result
  val to_err : ('a,T.e) result -> 'a t
  val err_of_option : T.e -> 'a option -> 'a t
end

module ErrS : sig
  type 'a t

  val fail : string -> 'a t
  val ret : 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val run : 'a t -> ('a,string) result
  val to_err : ('a,string) result -> 'a t
  val err_of_option : string -> 'a option -> 'a t
end

val filter_rev_map : ('a -> ('b,'c) result) -> 'a list -> 'b list
