(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Lambda

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
