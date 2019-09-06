(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Lambda
open Asttypes

type threshold = Percent of int | Hard of int

let h1' x = 1,[],x
let h1  x = h1' (Digest.string x)

let hash_string_lst is_sorting x xs =
  let sort xs =
    if is_sorting
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

let hash_incremental start xs =
  let p,lst,xs' =
    List.fold_right (fun (u,l,x) (v,l',xs) -> u+v,(u,x)::l@l',Digest.string (x^xs))
      xs start in
  1+p,lst,xs'

let hash_lst is_sorting f x xs =
  let res =
    List.fold_left
      (fun acc x -> f x :: acc)
      [] xs in
  hash_string_lst is_sorting x res

let hash_lst_anon is_sorting f xs = hash_lst is_sorting f "" xs

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

let hash_lambda is_sorting =
  let hash_string_lst = hash_string_lst is_sorting in
  let hash_lst_anon f = hash_lst_anon is_sorting f in
  let hash_lst f = hash_lst is_sorting f in
  let rec hash_lambda letbinds x =
    let hash_lambda' = hash_lambda letbinds in
  match x with
  | Lvar x ->
       begin
         match List.assoc_opt x letbinds with
         | None -> h1 "Lvar"
         | Some x' -> h1' x'
       end
  | Lconst _ -> h1 "Lconst"
  | Lapply x ->
     hash_incremental (hash_lambda' x.ap_func) (List.map hash_lambda' (x.ap_args))
  | Lfunction x ->
     hash_string_lst "Lfunction"
       [ hash_lambda' x.body ]
  | Llet (_,_,id,l,r) ->
     let ((_,_,h) as lefthash) = hash_lambda' l in
     hash_string_lst "Llet"
       [ lefthash
       ; hash_lambda ((id,h)::letbinds) r]
  | Lletrec (lst,l) ->
     hash_string_lst "Lletrec"
       [ hash_lst_anon (fun (_,x) -> hash_lambda' x) lst
       ; hash_lambda' l]
  | Lprim (_,lst,_) ->
     hash_string_lst "Lprim"
       [ hash_lst_anon hash_lambda' lst
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
  | Lassign (_,l) ->
     hash_string_lst "Lassign"
       [ hash_lambda' l
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
  in hash_lambda []

let sort_filter is_sorting threshold main_weight xs =
  let pred =
    match threshold with
    | Percent i -> fun u -> float_of_int u > ((float_of_int i) /. 100.) *. main_weight
    | Hard i -> fun u -> u > i in
  let filtered = List.filter (fun (u,_) -> pred u) xs in
  if is_sorting
  then List.sort compare filtered
  else filtered

let hash_lambda is_sorting threshold l =
  let main_weight,ss_arbres,h = hash_lambda is_sorting l in
  let fmain_weight = float_of_int main_weight in
  (main_weight,h), sort_filter is_sorting threshold fmain_weight ss_arbres

let fold_lambda lvar llet =
  let rec aux expr =
    let insnd lst = List.map (fun (e,x) -> e, aux x) lst in
    let inopt = function
    | None -> None
    | Some x -> Some (aux x) in
    match expr with
    | Lvar x -> lvar x
    | Lconst _ -> expr
    | Llet (k,e,ident,l,r) ->
       llet aux k e ident l r
  | Lapply x ->
     let ap_func = aux x.ap_func in
     let ap_args = List.map aux x.ap_args in
     Lapply {x with ap_func; ap_args }
  | Lfunction x ->
     let body = aux x.body in
     Lfunction {x with body}
  | Lletrec (lst,l) ->
     Lletrec (insnd lst, aux l)
  | Lprim (a,lst,b) ->
     Lprim (a,List.map aux lst, b)
  | Lstaticraise (a,lst) ->
     Lstaticraise (a,List.map aux lst)
  | Lifthenelse (i,f,e) ->
     Lifthenelse (aux i, aux f, aux e)
  | Lsequence (l,r) ->
     Lsequence (aux l, aux r)
  | Lwhile (l,r) ->
     Lwhile (aux l, aux r)
  | Lifused (i,l) ->
     Lifused (i, aux l)
#if OCAML_VERSION >= (4, 06, 0)
  | Lswitch (l,s,i) ->
     let sw_consts = insnd s.sw_consts in
     let sw_blocks = insnd s.sw_blocks in
     Lswitch (aux l, {s with sw_consts; sw_blocks}, i)
#else
  | Lswitch (l,s) ->
     let sw_consts = insnd s.sw_consts in
     let sw_blocks = insnd s.sw_blocks in
     Lswitch (aux l, {s with sw_consts; sw_blocks})
#endif
  | Lstringswitch (l,lst,opt,e) ->
     Lstringswitch (aux l, insnd lst, inopt opt, e)
  | Lassign (i,l) ->
     Lassign (i, aux l)
  | Levent (l,e) ->
     Levent (aux l, e)
  | Lstaticcatch (l,lst,r) ->
     Lstaticcatch (aux l, lst, aux r)
  | Ltrywith (l,i,r) ->
     Ltrywith (aux l, i, aux r)
  | Lfor (e,a,b,d,c) ->
     Lfor (e, aux a, aux b, d, aux c)
  | Lsend (a,b,c,d,e) ->
     Lsend (a, aux b, aux c, List.map aux d, e)
  in aux

(* Replace every occurence of ident by its body *)
let replace ident body =
  let lvar x =
    if x = ident
    then body
    else Lvar x in
  let llet aux a b c d e = Llet (a,b,c,aux d,aux e) in
  fold_lambda lvar llet

(* Is the definition inlineable ? *)
let inlineable x f =
  match x with
  | Alias -> true
  | Strict ->
     begin
       match f with
       | Lvar _ | Lconst _ -> true
       | _ -> false
     end
  | _  -> false

(* Inline all possible "let definitions"
   (that is, all "let definitions" without a side effet) *)
let inline_all =
  let lvar x = Lvar x in
  let llet aux k e ident l r =
    if inlineable k l
    then
      aux (replace ident l r)
    else
      Llet (k, e, ident, aux l, aux r) in
  fold_lambda lvar llet
