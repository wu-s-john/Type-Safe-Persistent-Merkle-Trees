type 'a option = 
    Some of 'a
  | None

module Either = struct
  type 'a t = 
      First of 'a |
      Second of 'a
end

module Tree = struct
  type 'a t = 
    | Leaf 
    | Node of 'a t * 'a * 'a t
end


(* EXAMPLE:  *)

(**
      2
    /   \
   1
     \
       3
*)

open Tree

let my_tree = 
  Node ((Node (Leaf, 1, Node (Leaf, 3, Leaf)) ), 2, Leaf)


(* What happens if we want to have trees that are perfectly balanced  *)


let get_value = 
  function 
  | Node (_, x, _) -> Some x
  | Leaf -> None
