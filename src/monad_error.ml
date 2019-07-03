(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type ('a,'b) either = Left of 'a | Right of 'b

let either f g = function
  | Left  x -> f x
  | Right x -> g x

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
end = struct
  type 'a t = (T.e,'a) either
  let fail e = Left e
  let ret x = Right x

  let map f = either (fun x -> Left x) (fun x -> Right (f x))
  let ( >>= ) x f = either (fun x -> Left x) f x

  let run x = x
  let to_err x = x
  let err_of_option e = function
    | None -> Left e
    | Some x -> Right x
end

module ErrS = Make(struct type e = string end)

let filter_rev_map f xs =
  List.fold_left (fun acc x -> either (fun _ -> acc) (fun x -> x :: acc) @@ f x) [] xs
