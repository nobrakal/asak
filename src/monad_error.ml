(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

let either f g = function
  | Ok    x -> f x
  | Error x -> g x

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

  let map f = either (fun x -> ret (f x)) fail
  let ( >>= ) x f = either f fail x

  let run x = x
  let to_err x = x
  let err_of_option e = function
    | None -> fail e
    | Some x -> ret x
end

module ErrS = Make(struct type e = string end)

let filter_rev_map f xs =
  List.fold_left (fun acc x -> either (fun x -> x :: acc) (fun _ -> acc) @@ f x) [] xs
