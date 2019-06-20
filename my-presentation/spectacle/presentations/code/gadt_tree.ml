module Peano = struct
  type z = Z

  type 'a t =   
    | S : 'a  -> ('a t)
end

let three : Peano.z Peano.t Peano.t Peano.t = 
  let open Peano in
  S (S (S Z))

module Balanced_tree = struct
  type ('a, 'height) t =
    | Leaf : ('a, Peano.z) t
    | Node : ('a, 'height) t * 'a * ('a, 'height) t -> 
        ('a, 'height Peano.t) t
end

let example =
  let open Balanced_tree in
  let leaf0= Node (Leaf, 0, Leaf) in
  let leaf3= Node (Leaf, 3, Leaf) in
  let leaf6= Node (Leaf, 6, Leaf) in
  let leaf7= Node (Leaf, 7, Leaf) in
  let level1_left =
    Node (leaf0, 1, leaf3)
  in
  let level1_right =
    Node (leaf6, 4, leaf7)
  in
  Node (level1_left, 2, level1_right)

let get_value : ('a, 'n Peano.t) Balanced_tree.t -> 'a =
  function (Node (_, x, _) ) -> x

let get_child : ('a, 'n Peano.t) Balanced_tree.t -> [`Left | `Right] -> ('a, 'n) Balanced_tree.t = 
  fun  (Node (l, _, r)) -> 
  function `Left -> l 
  | `Right -> r






