(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Lambda
open Asttypes

type threshold = Percent of int | Hard of int

type config =
  { should_sort : bool;
    hash_var : bool;
  }

type fingerprint = int * Digest.t

type hash = fingerprint * fingerprint list

let h1' x = 1,[],x
let h1  x = h1' (Digest.string x)

let hash_string_lst should_sort x xs =
  let sort xs =
    if should_sort
    then List.sort compare xs
    else xs in
  let p,lst,xs =
    List.fold_right
      (fun (u,l,x) (v,l',xs) -> u+v,(u,x)::l@l',x::xs)
      xs (0,[],[]) in
  let res =
    Digest.string @@
      String.concat "" @@
        sort @@
          if x = "" then xs else (Digest.string x::xs) in
  1+p,lst,res

let hash_prim x = h1 @@
  match x with
  | Pgetglobal id -> "Pgetglobal" ^ Ident.name id
#if OCAML_VERSION >= (5, 00, 0)
  | Pfield (i, _, _) ->
#else
  | Pfield i ->
#endif
  "Pfield" ^ string_of_int i
  | Pccall x -> "Pccall" ^ Primitive.native_name x
  | _ -> Printlambda.name_of_primitive x

let hash_lst should_sort f x xs =
  let res =
    List.fold_left
      (fun acc x -> f x :: acc)
      [] xs in
  hash_string_lst should_sort x res

let hash_lst_anon should_sort f xs = hash_lst should_sort f "" xs

let hash_case g f (i,x) =
  let a,b,c = f x in
  a+1,b,Digest.string (g i ^ c)

let hash_option f =
  function
  | None -> h1 "None"
  | Some x -> f x

let hash_direction x = h1 @@
  match x with
  | Upto -> "Upto"
  | Downto -> "Downto"

let hash_meth_kind x = h1 @@
   match x with
   | Self -> "Self"
   | Public -> "Public"
   | Cached -> "Cached"

let hash_lambda config x =
  let hash_string_lst = hash_string_lst config.should_sort in
  let hash_lst_anon f = hash_lst_anon config.should_sort f in
  let hash_lst f = hash_lst config.should_sort f in
  let hash_var prefix var =
    let str =
      if not config.hash_var
      then prefix
      else Ident.name var
    in h1 str
  in
  let rec hash_lambda' x =
    match x with
    | Lvar var ->
       hash_var "Lvar" var
    | Lconst _ ->
       h1 "Lconst"
    | Lapply x ->
       hash_string_lst "Lapply"
         (hash_lambda' x.ap_func :: (List.map hash_lambda' (x.ap_args)))
    | Lfunction x ->
       lfunc x
    | Llet (_,_,_,l,r) ->
       hash_string_lst "Llet"
         [ hash_lambda' l
         ; hash_lambda' r]
    | Lletrec (lst,l) ->
       hash_string_lst "Lletrec"
         [
#if OCAML_VERSION >= (4, 2, 0)
          hash_lst_anon (fun x -> lfunc x.def) lst
#else
          hash_lst_anon (fun x -> lfunc (snd x)) lst
#endif
         ; hash_lambda' l]
    | Lprim (prim,lst,_) ->
       hash_string_lst "Lprim"
         [ hash_prim prim;
           hash_lst_anon hash_lambda' lst
         ]
    | Lstaticraise (_,lst) ->
       hash_string_lst "Lstaticraise"
         [ hash_lst_anon hash_lambda' lst
         ]
    | Lifthenelse (i,f,e) ->
       hash_string_lst "Lifthenelse"
         [ hash_lambda' i
         ; hash_lambda' f
         ; hash_lambda' e
         ]
    | Lsequence (l,r) ->
       hash_string_lst "Lsequence"
         [ hash_lambda' l
         ; hash_lambda' r
         ]
    | Lwhile (l,r) ->
       hash_string_lst "Lwhile"
         [ hash_lambda' l
         ; hash_lambda' r
         ]
    | Lifused (_,l) ->
       hash_string_lst "Lifused"
         [ hash_lambda' l
         ]
#if OCAML_VERSION >= (4, 06, 0)
    | Lswitch (l,s,_) ->
#else
    | Lswitch (l,s) ->
#endif
       hash_string_lst "Lswitch"
       [ hash_lambda' l
       ; hash_lst (hash_case string_of_int hash_lambda') "sw_consts" s.sw_consts
       ; hash_lst (hash_case string_of_int hash_lambda') "sw_blocks" s.sw_blocks
       ; hash_option hash_lambda' s.sw_failaction
       ]
    | Lstringswitch (l,lst,opt,_) ->
       hash_string_lst "Lstringswitch"
         [ hash_lambda' l
         ; hash_lst (hash_case (fun x -> x) hash_lambda') "sw_consts" lst
         ; hash_option hash_lambda' opt
         ]
    | Lassign (var,l) ->
       hash_string_lst "Lassign"
         [ hash_var "Lvar" var
         ; hash_lambda' l
         ]
    | Levent (l,_) ->
       hash_string_lst "Levent"
         [ hash_lambda' l
         ]
    | Lstaticcatch (l,_,r) ->
       hash_string_lst "Lstaticcatch"
         [ hash_lambda' l
         ; hash_lambda' r
         ]
    | Ltrywith (l,_,r) ->
       hash_string_lst "Ltrywith"
         [ hash_lambda' l
         ; hash_lambda' r
         ]
    | Lfor (_,a,b,d,c) ->
       hash_string_lst "Lfor"
         [ hash_lambda' a
         ; hash_lambda' b
         ; hash_direction d
         ; hash_lambda' c
         ]
    | Lsend (m,a,b,xs,_) ->
       hash_string_lst "Lsend"
         [ hash_meth_kind m
         ; hash_lambda' a
         ; hash_lambda' b
         ; hash_lst_anon hash_lambda' xs
         ]
#if OCAML_VERSION >= (4, 13, 0)
    | Lmutvar var ->
       hash_var "Lmutvar" var
    | Lmutlet (_,_,l,r) ->
       hash_string_lst "Lmutlet"
         [ hash_lambda' l
         ; hash_lambda' r]
#endif
  and lfunc x =
    hash_string_lst "Lfunction"
      [ hash_lambda' x.body ]
  in hash_lambda' x

let sort_filter should_sort threshold main_weight xs =
  let pred =
    match threshold with
    | Percent i -> fun u -> float_of_int u > ((float_of_int i) /. 100.) *. main_weight
    | Hard i -> fun u -> u > i in
  let filtered = List.filter (fun (u,_) -> pred u) xs in
  if should_sort
  then List.sort compare filtered
  else filtered

let hash_lambda config threshold l =
  let main_weight,ss_arbres,h =
    hash_lambda config l in
  let fmain_weight = float_of_int main_weight in
  (main_weight,h), sort_filter config.should_sort threshold fmain_weight ss_arbres

let map_snd f xs = List.map (fun (x,y) -> x,f y) xs

let hash_all config hard_weight xs =
  map_snd (hash_lambda config (Hard hard_weight)) xs

let escape_hash ((p,h),xs) =
  (p,String.escaped h),List.map (fun (p,h) -> p,String.escaped h) xs
