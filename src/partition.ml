(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Monad_error
open ErrS
open Utils

open Parse_structure

type 'a partition =
  { bad_type : 'a list;
    clusters : 'a list Wtree.wtree list
  }

(* Search if a pattern has the right name *)
let has_name f x =
  let open Typedtree in
  match x.vb_pat.pat_desc with
  | Tpat_var (_,v) -> Asttypes.(v.txt) = f
  | _ -> false

let get_type_of_f_in_last f tree =
  let open Typedtree in
  let aux acc x =
    match x.str_desc with
    | Tstr_value (_,lst) ->
       begin
         match List.find_opt (has_name f) lst with
         | None -> acc
         | Some x -> ret x.vb_expr.exp_type
       end
    | _ -> acc
  in
  List.fold_left aux (fail @@ f ^ " not found") tree.str_items

let parse_all_implementations xs =
  let pred (t,save) =
    parsetree_of_string save
    >>= fun r -> ret (t,r)
  in filter_rev_map (fun x -> run @@ pred x) xs

let find_sol_type fun_name str =
  let found_type =
    parsetree_of_string str
    >>= type_with_init
    >>= get_type_of_f_in_last fun_name in
  match run found_type with
  | Left s -> failwith ("Error in solution: " ^ s)
  | Right x -> x

(* Test if two types are "equal" *)
let eq_type env t1 t2 =
  try Ctype.unify env t1 t2; true with
  | Ctype.Unify _ -> false

(* Partition the codes between those who have the function with
   the right name and the right type, and the others *)
let partition_funexist sol_type fun_name =
  let init_env = init_env () in
  let eq_type = eq_type init_env in
  let pred lst =
    let tree =
      type_with_init lst
      >>= fun t ->
      get_type_of_f_in_last fun_name t
      >>= fun x ->
      if eq_type sol_type x
      then ret (last lst, List.hd @@ rev_lambdas_of_lst "" t)
      else fail "bad type" in
    run tree in
  let aux (bad,good) (n,x) =
    match pred x with
    | Left _ -> (n::bad, good)
    | Right x -> (bad, (n,x)::good)
  in List.fold_left aux ([],[])

let hm_part prof m =
  let threshold = Lambda_utils.Percent prof in
  let lst =
    List.fold_left
      (fun acc (t,(_,x)) ->
        let hash,lst = Lambda_utils.hash_lambda true threshold x in
        (t,(hash::lst))::acc
      ) [] m in
  Clustering.cluster lst

let create prof fun_name sol codes =
  let codes = parse_all_implementations codes in
  let sol_type = find_sol_type fun_name sol in
  let bad_type,funexist = partition_funexist sol_type fun_name codes in
  let clusters = hm_part prof funexist in
  {bad_type; clusters}
