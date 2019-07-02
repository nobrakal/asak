(* This file is part of asak.
 *
 * Copyright (C) 2019 IRIF / OCaml Software Foundation.
 *
 * asak is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Monad_error
open Err

let parsetree_of_string str =
  try
    let without_directives =
      String.concat ";;" @@
        List.filter
          (fun x -> let x = String.trim x in String.length x > 0 && x.[0] != '#') @@
          Str.split (Str.regexp_string ";;") str in
    ret (Parse.implementation (Lexing.from_string without_directives))
  with
  | Lexer.Error _ | Syntaxerr.Error _ -> fail

let init_env () =
  Compmisc.init_path true;
  Compmisc.initial_env ()

let extract_typedtree =
#if OCAML_VERSION >= (4, 08, 0)
  fun (s,_,_,_) -> s
#else
  fun (s,_,_) -> s
#endif

let type_with_init lst =
  try ret (extract_typedtree @@ Typemod.type_structure (init_env ()) lst Location.none)
  with Typetexp.Error _ | Typecore.Error _ -> fail

let lambda_of_typedtree lst =
  Lambda_utils.inline_all @@
    Simplif.simplify_lambda "" @@
      Translmod.transl_toplevel_definition lst
