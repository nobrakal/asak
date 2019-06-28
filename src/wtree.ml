(* Type for weighted trees *)
type 'a wtree =
  | Node of (float * 'a wtree * 'a wtree)
  | Leaf of 'a

let fold_tree n l =
  let rec aux = function
    | Leaf a -> l a
    | Node (f,a,b) -> n f (aux a) (aux b)
  in aux

let size_of_tree f =
  fold_tree (fun _ a b -> 1 + a + b) f
