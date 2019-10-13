(** Inline all possible (ie. without side effect) expressions in a lambda expression. *)
val inline_all : Lambda.lambda -> Lambda.lambda
