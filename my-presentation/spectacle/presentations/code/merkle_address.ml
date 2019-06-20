module Merkle_address = struct
  type ('height, 'depth) t = (bool, 'height, 'depth) Bounded_vector.t

  let parent : ('height, 'depth Peano.t) t -> ('height Peano.t, 'depth) t = 
    function
    | _::xs -> xs
  
  let child : ('height Peano.t, 'depth) t -> bool -> ('height, 'depth Peano.t) t =
    fun xs x -> x :: xs

  let sibling : ('height, 'depth Peano.t) t -> ('height, 'depth Peano.t) t =
    function 
    |  x::xs ->  (not x) :: xs


  module E = struct

    module T = struct
      type t =
          E : ('height, 'depth) T.t -> t
    end

    include T
  end
end

open Merkle_address

let path1 = [false; true]

let path2 = [true]
