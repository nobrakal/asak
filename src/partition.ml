(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Monad_error
open Err

open Parse_structure

type 'a partition =
  { bad_type : 'a list;
    clusters : 'a list Wtree.wtree list
  }

let take_until_last p =
  let rec aux = function
  | [] -> None
  | x::xs ->
     match aux xs with
     | None ->
        if p x
        then Some [x]
        else None
     | Some xs -> Some (x::xs)
  in aux

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
  List.fold_left aux fail tree.str_items

(* "Cut" a structure, with the last definition being the searched function *)
let find_func f xs  =
  let open Parsetree in
  let pred c =
    match c.pstr_desc with
    | Pstr_value (_,(x::_)) ->
       begin
         match x.pvb_pat.ppat_desc with
         | Ppat_var v -> Asttypes.(v.txt) = f
         | _ -> false
       end
    | _ -> false
  in
  to_err (take_until_last pred xs)

let parse_all_implementations fun_name =
  let pred (t,save) =
    parsetree_of_string save
    >>= find_func fun_name
    >>= fun r -> ret (t,r)
  in filter_rev_map pred

let rec last = function
  | [] -> failwith "last"
  | [x] -> x
  | _::xs -> last xs

let find_sol_type str fun_name =
  let found_type =
    parsetree_of_string str
    >>= find_func fun_name
    >>= type_with_init
    >>= get_type_of_f_in_last fun_name in
  match run found_type with
  | None -> failwith "Required function not implemented in solution"
  | Some x -> x

(* Get the last element of a list of lambda expression *)
let rec get_last_of_seq = function
  | Lambda.Lsequence (_,u) -> get_last_of_seq u
  | x -> x

(* Test if two types are "equal" *)
let eq_type env t1 t2 =
  try Ctype.unify env t1 t2; true with
  | Ctype.Unify _ -> false

(* Partition the codes between those who have the function with
   the right name and the right type, and the others *)
let partition_FunExist sol_type fun_name =
  let init_env = init_env () in
  let eq_type = eq_type init_env in
  let pred lst =
    let tree =
      type_with_init lst
      >>= fun t ->
      get_type_of_f_in_last fun_name t
      >>= fun x ->
      if eq_type sol_type x
      then ret (last lst, get_last_of_seq @@ lambda_of_typedtree t)
      else fail in
    run tree in
  let aux (bad,good) (n,x) =
    match pred x with
    | None -> (n::bad, good)
    | Some x -> (bad, (n,x)::good)
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
  let codes = parse_all_implementations fun_name codes in
  let sol_type = find_sol_type fun_name sol in
  let bad_type,funexist = partition_FunExist sol_type fun_name codes in
  let clusters = hm_part prof funexist in
  {bad_type; clusters}
