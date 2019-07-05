(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

let either f g = function
  | Error x -> f x
  | Ok    x -> g x

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
end = struct
  type 'a t = ('a,T.e) result
  let fail e = Error e
  let ret x = Ok x

  let map f = either fail (fun x -> ret (f x))
  let ( >>= ) x f = either fail f x

  let run x = x
  let to_err x = x
  let err_of_option e = function
    | None -> fail e
    | Some x -> ret x
end

module ErrS = Make(struct type e = string end)

let filter_rev_map f xs =
  List.fold_left (fun acc x -> either (fun _ -> acc) (fun x -> x :: acc) @@ f x) [] xs
