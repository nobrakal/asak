(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Monad_error.ErrS

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
    ret
      (extract_typedtree @@ Typemod.type_structure (init_env ?to_open ()) lst Location.none)
  with Typetexp.Error _ | Typecore.Error _ -> fail "type error"

let lambda_of_typedtree lst =
  Lambda_utils.inline_all @@
    Simplif.simplify_lambda "" @@
      Translmod.transl_toplevel_definition lst
