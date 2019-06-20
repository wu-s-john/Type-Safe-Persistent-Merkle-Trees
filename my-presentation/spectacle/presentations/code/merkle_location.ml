module Location = struct
    type ('typ, 'depth) t =
      | Account : (z, Max_height.t) Merkle_address.t -> (Account.t, Max_height.t) t
      | Hash : ('height, 'depth) Merkle_address.t -> (Hash.t, 'depth) t

  type ('typ, 'depth) location = ('typ, 'depth) t

  module E = struct
    type t = 
        E : (Hash.t, 'depth) location -> t
  end
end