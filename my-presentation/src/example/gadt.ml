module Peano = struct
  type z = Z

  type 'a t =   
    | S : 'a  -> ('a t)
end

let three : Peano.z Peano.t Peano.t Peano.t = 
  let open Peano in
  S (S (S Z))


(* What if we want to construct a balanced tree *)

module Balanced_tree = struct
  type ('a, _) t =
    | Leaf : ('a, Peano.z) t
    | Node : ('a, 'n) t * 'a * ('a, 'n) t -> 
        ('a, 'n Peano.t) t
end

(*  You cannot build this type of tree as it will give you a type error *)
(**
      2
    /   \
   1
     \
       3
*)

(* Cause a compiler error *)
(* 

let my_tree = 
   let open Balanced_tree in
   Node ((Node (Leaf, 1, Node (Leaf, 3, Leaf)) ), 2, Leaf)    
*)


(**
          2
     /         \\
    1            4
   /   \\         /  \\
   0      3      6     7
*)

let my_balanced_tree =
  let open Balanced_tree in
  let level1_left =
    Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 3, Leaf))
  in
  let level1_right =
    Node (Node (Leaf, 6, Leaf), 4, Node (Leaf, 7, Leaf))
  in

  Node (level1_left, 2, level1_right)

let get_value : ('a, 'n Peano.t) Balanced_tree.t -> 'a =
  function (Node (_, x, _) ) -> x