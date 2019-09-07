(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Typedtree

open Monad_error.ErrS

let filter_map f xs =
  let aux x acc =
    match f x with
    | None -> acc
    | Some x -> x::acc
  in List.fold_right aux xs []

let parsetree_of_string str =
  try
    let without_directives =
      String.concat ";;" @@
        List.filter
          (fun x -> let x = String.trim x in String.length x > 0 && x.[0] != '#') @@
          Str.split (Str.regexp_string ";;") str in
    ret (Parse.implementation (Lexing.from_string without_directives))
  with
  | Lexer.Error _ | Syntaxerr.Error _ -> fail "parse error"

let init_env ?to_open () =
  let old_modules = !Clflags.open_modules in
  let update_opened () =
    match to_open with
    | None -> ()
    | Some x -> Clflags.open_modules := x::!Clflags.open_modules in
  Compmisc.init_path true;
  update_opened ();
  let env = Compmisc.initial_env () in
  Clflags.open_modules := old_modules;
  env

let extract_typedtree =
#if OCAML_VERSION >= (4, 08, 0)
  fun (s,_,_,_) -> s
#else
  fun (s,_,_) -> s
#endif

let type_with_init ?to_open lst =
  try
    ret @@
      extract_typedtree @@
        Typemod.type_structure (init_env ?to_open ()) lst Location.none
  with Typetexp.Error _ | Typecore.Error _ -> fail "type error"

let lambda_of_expression expr =
  Lambda_utils.inline_all @@
    Simplif.simplify_lambda "" @@
      Translcore.transl_exp expr

let get_name_of_pat pat =
  match pat.pat_desc with
  | Tpat_var(id, _) -> Some id
  | Tpat_alias(_, id, _) -> Some id
  | _ -> None

let has_name f x =
  match get_name_of_pat x.vb_pat with
  | None -> false
  | Some id -> Ident.name id = f

let list_find_map f =
  let aux acc x =
    match acc with
    | None -> f x
    | _ -> acc
  in List.fold_left aux None

let get_specific_lambda_of_typedtree name structure =
  let pred_binding x =
    if has_name name x
    then Some x.vb_expr
    else None in
  let pred x =
    match x.str_desc with
    | Tstr_value (_,xs) -> list_find_map pred_binding xs
    | _ -> None in
  match list_find_map pred structure.str_items with
  | None -> fail "get_specific_lambda_of_typedtree: function not found"
  | Some item -> ret @@ lambda_of_expression item

let find_let_in_parsetree_items f =
  let open Parsetree in
  let pred_binding x =
    match x.pvb_pat.ppat_desc with
    | Ppat_var v -> Asttypes.(v.txt) = f
    | _ -> false in
  let pred x =
    match x.pstr_desc with
    | Pstr_value (_,xs) -> List.exists pred_binding xs
    | _ -> false in
  List.find_opt pred

let rec read_module_expr prefix m =
  match m.mod_desc with
  | Tmod_structure structure -> read_structure prefix structure
  | Tmod_functor (_,_,_,m) -> read_module_expr prefix m
  | _ -> []

and read_value_binding prefix x =
  match get_name_of_pat x.vb_pat with
  | Some name -> Some
     (prefix ^ "." ^ (Ident.name name), name, lambda_of_expression x.vb_expr)
  | None -> None

and read_item_desc prefix x =
  let read_module_expr m =
     read_module_expr (prefix ^ "." ^ Ident.name m.mb_id) m.mb_expr in
  match x.str_desc with
  | Tstr_value (_,xs) -> filter_map (read_value_binding prefix) xs
  | Tstr_module m -> read_module_expr m
  | Tstr_recmodule xs ->
     List.flatten @@
       List.map read_module_expr xs
  | _ -> []

and read_structure prefix structure =
  List.flatten @@
    List.map (read_item_desc prefix) structure.str_items
