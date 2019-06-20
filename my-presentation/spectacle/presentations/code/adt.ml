type 'a option = 
    Some of 'a
  | None

module Tree = struct
  type 'a t = 
    | Leaf 
    | Node of 'a t * 'a * 'a t
end

open Tree
let example_tree = 
  Node ((Node (Leaf, 1, Node (Leaf, 3, Leaf)) ), 2, Leaf)

let get_value = 
  function 
  | Node (_, x, _) -> Some x
  | Leaf -> None

let get_child = 
  function (Leaf, _) -> None
  | (Node (l, _, _), `Left) -> Some l
  | (Node (_, _, r), `Right) -> Some r
















