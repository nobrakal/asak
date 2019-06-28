(* Type for weighted trees *)
type 'a wtree =
  | Node of (float * 'a wtree * 'a wtree)
  | Leaf of 'a

val fold_tree : (float -> 'b -> 'b -> 'b) -> ('a -> 'b) -> 'a wtree -> 'b

val size_of_tree : ('a -> int) -> 'a wtree -> int
