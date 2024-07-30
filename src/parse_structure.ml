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

let init_path () =
#if OCAML_VERSION >= (4, 09, 0)
   Compmisc.init_path ()
#else
   Compmisc.init_path true
#endif

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

let init_env () =
  let old_modules = !Clflags.open_modules in
  init_path ();
  let env = Compmisc.initial_env () in
  Clflags.open_modules := old_modules;
  env

let extract_typedtree =
#if OCAML_VERSION >= (4, 14, 0)
  fun (s,_,_,_,_) -> s
#elif OCAML_VERSION >= (4, 08, 0)
  fun (s,_,_,_) -> s
#else
  fun (s,_,_) -> s
#endif

let type_with_init lst =
  try
    ret @@
      extract_typedtree @@
        Typemod.type_structure (init_env ()) lst
#if OCAML_VERSION < (4, 12, 0)
    Location.none
#endif
  with Typetexp.Error _ | Typecore.Error _ -> fail "type error"

let simplify_lambda lambda =
#if OCAML_VERSION >= (4, 09, 0)
  Simplif.simplify_lambda lambda
#else
  Simplif.simplify_lambda "" lambda
#endif

let transl_exp expr =
#if OCAML_VERSION >= (4, 12, 0)
  Translcore.transl_exp ~scopes:Debuginfo.Scoped_location.empty_scopes expr
#elif OCAML_VERSION >= (4, 11, 0)
  Translcore.transl_exp ~scopes:[] expr
#else
  Translcore.transl_exp expr
#endif

let lambda_of_expression ?name expr =
  Lambda_normalization.normalize_local_variables ?name @@
    Lambda_normalization.inline_all @@
      simplify_lambda @@
        transl_exp expr

let get_name_of_pat pat =
  match pat.pat_desc with
#if OCAML_VERSION >= (5, 2, 0)
  | Tpat_var(id, _, _) -> Some id
  | Tpat_alias(_, id, _, _) -> Some id
#else
  | Tpat_var(id, _) -> Some id
  | Tpat_alias(_, id, _) -> Some id
#endif
  | _ -> None

let get_name f x =
  match get_name_of_pat x.vb_pat with
  | Some id when (Ident.name id = f) -> Some id
  | _ -> None

let has_name f x =
  match get_name f x with
  | Some _ -> true
  | None -> false

let list_find_map f =
  let aux acc x =
    match acc with
    | None -> f x
    | _ -> acc
  in List.fold_left aux None

let get_specific_lambda_of_typedtree name structure =
  let pred_binding x =
    match get_name name x with
    | Some name -> Some (name, x.vb_expr)
    | None -> None in
  let pred x =
    match x.str_desc with
    | Tstr_value (_,xs) -> list_find_map pred_binding xs
    | _ -> None in
  match list_find_map pred structure.str_items with
  | None -> fail "get_specific_lambda_of_typedtree: function not found"
  | Some (name,item) -> ret @@ lambda_of_expression ~name item

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

let rec read_module_expr ~prefix m =
  match m.mod_desc with
  | Tmod_structure structure -> read_structure_with_loc ~prefix structure
#if OCAML_VERSION >= (4, 10, 0)
  | Tmod_functor (_,m) ->
#else
  | Tmod_functor (_,_,_,m) ->
#endif
      read_module_expr ~prefix m
  | _ -> []

and read_value_binding ~prefix x =
  match get_name_of_pat x.vb_pat with
  | Some name ->
     let name_s = prefix ^ "." ^ (Ident.name name) in
     Some ((name_s , x.vb_pat.pat_loc), lambda_of_expression ~name x.vb_expr)
  | None -> None

and read_item_desc ~prefix x =
  let read_module_expr m =
    let mid =
#if OCAML_VERSION >= (4, 10, 0)
      Option.value ~default:"" (Option.map Ident.name m.mb_id)
#else
      Ident.name m.mb_id
#endif
    in
    let prefix = prefix ^ "." ^ mid in
     read_module_expr ~prefix m.mb_expr in
  match x.str_desc with
  | Tstr_value (_,xs) -> filter_map (read_value_binding ~prefix) xs
  | Tstr_module m -> read_module_expr m
  | Tstr_recmodule xs ->
     List.flatten @@
       List.map read_module_expr xs
  | _ -> []

and read_structure_with_loc ?prefix structure =
  let prefix =
    match prefix with
    | None -> ""
    | Some prefix -> prefix in
  List.flatten @@
    List.map (fun x -> read_item_desc ~prefix x) structure.str_items

let read_structure ?prefix structure =
  List.map (fun ((x,_),y) -> x,y) (read_structure_with_loc ?prefix structure)

let read_string str =
  let t = parsetree_of_string str >>= type_with_init in
  match run t with
  | Error e -> failwith e
  | Ok t -> read_structure t
