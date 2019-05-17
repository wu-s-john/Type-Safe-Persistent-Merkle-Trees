open Core_kernel

type z = Z

type 'a s = 
  | S : 'a  -> ('a s) 

type max_height = z s s s s

type ('elem, 'height, 'depth) bounded_vector =
  | [] : ('elem, max_height, z) bounded_vector 
  | ( :: ) : 'elem * ('elem, 'height s, 'depth) bounded_vector  -> ('elem, 'height, 'depth s) bounded_vector


let rec length : type height depth. ('elem, height, depth) bounded_vector -> int = 
  function 
  | [] -> 0
  | _ :: xs -> 1 + length xs



type ('height, 'depth) merkle_address = (bool, 'height, 'depth) bounded_vector

let parent : ('height, 'depth s) merkle_address -> ('height s, 'depth) merkle_address = 
  function
  | _::xs -> xs

let child : ('height s, 'depth) merkle_address -> bool -> ('height, 'depth s) merkle_address =
  fun xs x -> x :: xs

let sibling : ('height s, 'depth) merkle_address -> ('height s, 'depth) merkle_address =
  function x::xs ->  (not x) :: xs

let foo = [1; 2; 3; 4] 


module Public_key = struct
  type t = String.t [@@deriving hash]
end

module Account = struct
  type t = {
    public_key : Public_key.t;
    balance : int
  } [@@deriving hash]
end

module Hash = struct
  type t = int [@@deriving hash]
end

module Location = struct
  type ('typ, 'depth) t =
    | Account : (z, max_height) merkle_address -> (Account.t, max_height) t
    | Hash : ('height, 'depth) merkle_address -> (Hash.t, 'depth) t
end

module Database_lookup = struct
  (* Last value *)
  type _ t = 
    | Account_location : Public_key.t -> ((Account.t, max_height) Location.t) t
    | Account : (Account.t, max_height) Location.t -> Account.t t
    | Hash : (Hash.t, 'depth) Location.t -> Hash.t t
    | Last_allocated : ((Account.t, max_height) Location.t) t
end

