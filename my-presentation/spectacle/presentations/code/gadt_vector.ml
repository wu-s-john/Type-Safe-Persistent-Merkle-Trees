module Vector = struct
  type ('elem, 'size) t =
    | [] : ('elem, Peano.z) t
    | ( :: ) : 'elem * ('elem, 'n) t ->  ('elem, 'n Peano.t) t
end

let three_elem_vector: (int, Peano.z Peano.t Peano.t Peano.t) Vector.t = [1; 2; 3]

module Max_height = struct
    (* Max height is 4 *)
    type t = Peano.z Peano.t Peano.t Peano.t Peano.t
end

module Bounded_vector = struct
  type ('elem, _, _) t =
    | [] : ('elem, Max_height.t, Peano.z) t
    | ( :: ) : 'elem * ('elem, 'height Peano.t, 'depth) t ->  ('elem, 'height, 'depth Peano.t) t
end

let valid_vector = [1; 2; 3]

let illegal_vector = [1; 2; 3; 4; 5]
