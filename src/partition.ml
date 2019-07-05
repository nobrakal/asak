(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Monad_error
open ErrS

open Parse_structure

type 'a partition =
  { bad_type : 'a list;
    clusters : ('a * string) list Wtree.wtree list
  }

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
  | Error s -> failwith ("Error in solution: " ^ s)
  | Ok x -> x

let eq_type env t1 t2 =
  try Ctype.unify env t1 t2; true with
  | Ctype.Unify _ -> false

let partition_funexist sol_type fun_name =
  let init_env = init_env () in
  let eq_type = eq_type init_env in
  let pred lst =
    let tree =
      type_with_init lst
      >>= fun t ->
      get_type_of_f_in_last fun_name t
      >>= fun x ->
      if not (eq_type sol_type x)
      then
        fail "bad type"
      else
        (get_specific_lambda_of_typedtree fun_name t
         >>= fun lambda ->
         match find_let_in_parsetree_items fun_name lst with
         | None -> fail "cannot find function in parsetree"
         | Some impl -> ret (impl, lambda)) in
    run tree in
  let aux (bad,good) (n,x) =
    match pred x with
    | Error _ -> (n::bad, good)
    | Ok x -> (bad, (n,x)::good)
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

let add_impl_example m cluster =
  let open Wtree in
  let string_of_impl x = Pprintast.string_of_structure [x] in
  Wtree.fold_tree
    (fun a b c -> Node (a,b,c))
    (fun x ->
      Leaf
        (List.map
           (fun e ->
             let (_,(ref_impl,_)) = List.find (fun (t,_) -> t = e) m in
             (e, string_of_impl ref_impl)
           ) x)
    )
    cluster

let create prof fun_name sol codes =
  let codes = parse_all_implementations codes in
  let sol_type = find_sol_type fun_name sol in
  let bad_type,funexist = partition_funexist sol_type fun_name codes in
  let clusters = List.map (add_impl_example funexist) @@ hm_part prof funexist in
  {bad_type; clusters}
