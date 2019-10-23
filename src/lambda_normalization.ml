(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Lambda

let map_snd aux = List.map (fun (e,x) -> e, aux x)

let map_opt aux = function
  | None -> None
  | Some x -> Some (aux x)

let fold_lambda lvar llet =
  let rec aux expr =
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
     Lletrec (map_snd aux lst, aux l)
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
     let sw_consts = map_snd aux s.sw_consts in
     let sw_blocks = map_snd aux s.sw_blocks in
     Lswitch (aux l, {s with sw_consts; sw_blocks}, i)
#else
  | Lswitch (l,s) ->
     let sw_consts = map_snd s.sw_consts in
     let sw_blocks = map_snd s.sw_blocks in
     Lswitch (aux l, {s with sw_consts; sw_blocks})
#endif
  | Lstringswitch (l,lst,opt,e) ->
     Lstringswitch (aux l, map_snd aux lst, map_opt aux opt, e)
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

let extract_params_name xs =
#if OCAML_VERSION >= (4, 08, 0)
  List.map fst xs
#else
  xs
#endif

let create_ident x =
#if OCAML_VERSION >= (4, 08, 0)
  Ident.create_local x
#else
  Ident.create x
#endif

let normalize_local_variables x =
  let rec aux i letbinds x =
    let aux' = aux i letbinds in
    match x with
    | Lvar var ->
       begin
         match List.assoc_opt var letbinds with
         | None -> x
         | Some x -> Lvar (create_ident (string_of_int x))
       end
    | Lconst _ -> x
    | Lapply x ->
       Lapply {x with ap_func=aux' x.ap_func; ap_args=List.map aux' x.ap_args}
    | Lfunction x ->
       let params = extract_params_name x.params in
       let (i,letbinds) =
         List.fold_right (fun id (i,acc) -> (i+1, (id,i)::acc)) params (i,letbinds) in
       Lfunction {x with body=aux i letbinds x.body}
    | Llet (a,b,id,l,r) ->
       Llet (a,b,id,aux' l, aux (i+1) ((id,i)::letbinds) r)
    | Lletrec (lst,l) ->
       let (i,letbinds) =
         List.fold_right (fun (id,_) (i,acc) -> (i+1),(id,i)::acc) lst (i,letbinds) in
       Lletrec (List.map (fun (t,x) -> t,aux i letbinds x) lst, aux i letbinds l)
    | Lprim (a,b,c) ->
       Lprim (a, List.map aux' b,c)
    | Lstaticraise (a,b) ->
       Lstaticraise (a,List.map aux' b)
    | Lifthenelse (i,f,e) ->
       Lifthenelse (aux' i, aux' f, aux' e)
    | Lsequence (l,r) ->
       Lsequence (aux' l, aux' r)
    | Lwhile (l,r) ->
       Lwhile (aux' l, aux' r)
    | Lifused (a,b) ->
       Lifused (a, aux' b)
#if OCAML_VERSION >= (4, 06, 0)
    | Lswitch (l,s,u) ->
       let s =
         {s with sw_consts = map_snd aux' s.sw_consts;
                 sw_blocks = map_snd aux' s.sw_blocks} in
       Lswitch (aux' l, s, u)
#else
    | Lswitch (l,s) ->
        let s =
         {s with sw_consts = map_snd aux' s.sw_consts;
                 sw_blocks = map_snd aux' s.sw_blocks} in
        Lswitch (aux' l, s, u)
#endif
    | Lstringswitch (l,lst,opt,loc) ->
       Lstringswitch (aux' l, map_snd aux' lst, map_opt aux' opt, loc)
    | Lassign (a,b) ->
       Lassign (a, aux' b)
    | Levent (a,b) ->
       Levent (aux' a, b)
    | Lstaticcatch (a,b,c) ->
       Lstaticcatch (aux' a, b, aux' c)
    | Ltrywith (l,id,r) ->
       Ltrywith (aux' l, id, aux (i+1) ((id,i)::letbinds) r)
    | Lfor (id,a,b,d,c) ->
       Lfor (id,aux' a, aux' b, d, aux (i+1) ((id,i)::letbinds) c)
    | Lsend (a,b,c,d,e) ->
       Lsend (a, aux' b, aux' c, List.map aux' d, e)
  in aux 0 [] x
