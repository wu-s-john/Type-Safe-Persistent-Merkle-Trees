module Vector = struct
  type ('elem, 'size) t =
    | [] : ('elem, Peano.z) t
    | ( :: ) : 'elem * ('elem, 'height) t ->  ('elem, 'height Peano.t) t
end

let three_elem_vector: (int, Peano.z Peano.t Peano.t Peano.t) Vector.t = [1; 2; 3]

module Max_height = struct
    (* Max height is 4 *)
    type t = Peano.z Peano.t Peano.t Peano.t Peano.t
end

module Bounded_vector = struct
  type ('elem, 'remaining, 'size) t =
    | [] : ('elem, Max_height.t, Peano.z) t
    | ( :: ) : 'elem * ('elem, 'remaining Peano.t, 'size) t ->  ('elem, 'remaining, 'size Peano.t) t
end

let valid_bounded_vector : (_, Peano.z Peano.t, Peano.z Peano.t Peano.t Peano.t) Bounded_vector.t = [1; 2; 3]

let illegal_bounded_vector : (_, _, _) Bounded_vector.t = [1; 2; 3; 4; 5]
