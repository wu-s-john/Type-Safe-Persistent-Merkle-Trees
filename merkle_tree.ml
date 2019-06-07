open Core


type z = Z

module Peano = struct

  type 'a t =   
    | S : 'a  -> ('a t)

  type nonrec z = z
end

module Max_height = struct
  open Peano 
  type nonrec t = z t t t
end

module Vector = struct

  type ('elem, _) t =
    | [] : ('elem, Peano.z) t
    | ( :: ) : 'elem * ('elem, 'n) t ->  ('elem, 'n Peano.t) t


  let three_elem_vector: (int, z Peano.t Peano.t Peano.t) t = [1; 2; 3]
end

module OneToThree = struct
  type 'a t = 
    | One : (Peano.z Peano.t)t
    | Two : (Peano.z Peano.t Peano.t)t
    | Three : (Peano.z Peano.t Peano.t Peano.t)t
end

module OneToThreeVector = struct
  type nonrec ('elem, 'n) t = ('elem, 'n) Vector.t * 'n OneToThree.t

  let x : (int, z Peano.t Peano.t Peano.t) t = 
    Vector.([1; 2; 3]), Three

  (* Will have a compiling error *)
  let three_elem_vector_error : (int, z Peano.t Peano.t Peano.t) t = 
    Vector.([1; 2; 3]), Two
end

module Bounded_Peano = struct
  type (_, _) t =   
    | Z: (Peano.z, Max_height.t) t
    | S : ('num, 'remaining Peano.t) t -> ('num Peano.t, 'remaining) t


  let x = S (S Z)

  let max_value : (Max_height.t, Peano.z) t = 
    S (S (S (Z)))


  let rec to_int : type num remaining. (num, remaining) t -> int = function
    | Z -> 0
    | S prev -> 1 + to_int prev
end

module Bounded_vector = struct
  (* TODO:  Optimize this so that has height information as well  *)

  module T  = struct
    type ('elem, _, _) t =
      | [] : ('elem, Max_height.t, Peano.z) t
      | ( :: ) : 'elem * ('elem, 'height Peano.t, 'depth) t ->  ('elem, 'height, 'depth Peano.t) t
  end
  include T 

  (* TODO: explain why you have to do it like that *)
  let rec map : type height depth. ('a -> 'b) -> ('a, height, depth) t -> ('b, height, depth) t =
    fun f -> 
    function 
    | [] -> []
    | x::xs -> f x :: (map f xs)

  let rec fold_left : type height depth. init: 'b -> f: ('a -> 'b -> 'b) -> ('a, height, depth) t -> 'b  =
    fun ~init ~f ->
    function 
    | [] -> init
    | x::xs -> f x (fold_left ~init ~f xs)

  let to_list : type height depth. ('a, height, depth) t ->  'a list = 
    fun vector -> 
    let empty_list : 'a list = List.([]) in
    fold_left vector ~init:empty_list ~f:List.cons 

  let sexp_of_t : type height depth. ('a -> Sexp.t) -> ('a, height, depth) t -> Sexp.t = fun f t ->
    sexp_of_list f @@ to_list t

  (* TODO: Incapsulate size into each element of the vector *)
  let rec get_peano : type height depth. ('a, height, depth) t -> (depth, height) Bounded_Peano.t =
    function 
    | [] -> Bounded_Peano.Z
    | _::xs -> Bounded_Peano.S (get_peano xs)

  let rec of_list : type height depth. 'elem list -> (depth, height) Bounded_Peano.t -> ('a, height, depth) t =
    fun list depth -> 
    match (depth, list) with
    | (Bounded_Peano.Z, []) -> []
    | (Bounded_Peano.S num, (x::xs) ) -> x :: (of_list xs num)
    | _ -> failwith "Edge case not supported :("

  module E = struct
    type 'elem t =
      | E : ('elem, 'height, 'depth) T.t  -> 'elem t
  end

  let length : type height depth. ('elem, height, depth) t -> int = 
    fun t ->  fold_left ~init:0 ~f:(fun _ -> (+) 1) t

  module Make_binable(Input: sig
      type height

      type depth

      val peano : (depth, height) Bounded_Peano.t
    end) : Binable.S1 with type 'a t := ('a, Input.height, Input.depth) t = struct

    open Input

    module Binable_vector = Bin_prot.Utils.Make_binable1(struct 

        module Binable = List

        type nonrec 'b t = ('b, height, depth ) t

        let to_binable (t: 'b t) : 'b Binable.t = to_list t

        let of_binable list = of_list list peano

      end)

    include Binable_vector
  end

  let bin_t : type height depth. (depth, height) Bounded_Peano.t -> ('a, ('a, height, depth) t) Bin_prot.Type_class.S1.t = 
    fun peano -> 

    let (module M) = (module struct

      type nonrec 'b t = ('b, height, depth) t

      include Make_binable(struct 
          type nonrec height = height

          type nonrec  depth = depth

          let peano = peano
        end)
    end : Bin_prot.Binable.S1) in

    (* Lol Today I broke the godly OCaml Compiler *)
    Obj.magic M.bin_t
end

module Merkle_address = struct
  module T = struct 
    type ('height, 'depth) t = (bool, 'height, 'depth) Bounded_vector.t
  end

  include T

  let sexp_of_t : type height depth. (sexp_bool, height, depth) Bounded_vector.t -> Sexp.t = 
    fun bounded_vector -> 
    Bounded_vector.sexp_of_t sexp_of_bool bounded_vector

  let parent : ('height, 'depth Peano.t) t -> ('height Peano.t, 'depth) t = 
    function
    | _::xs -> xs

  let child : ('height Peano.t, 'depth) t -> bool -> ('height, 'depth Peano.t) t =
    fun xs x -> x :: xs

  let sibling : ('height, 'depth Peano.t) t -> ('height, 'depth Peano.t) t =
    function 
    |  x::xs ->  (not x) :: xs

  (* TODO: This is really slow optimize *)
  let serialize (t : ('height, 'depth) t) =  
    Bounded_vector.fold_left t ~init:"" ~f:(fun x acc -> if x then String.concat ~sep:"" ["1"; acc] else String.concat ~sep:"" ["0"; acc] )

  let bin_t : (('depth, 'height) Bounded_Peano.t) -> ('height, 'depth) t Bin_prot.Type_class.t = fun t -> 
    Bounded_vector.bin_t t Bool.bin_t

  let order_siblings : ('height, 'depth Peano.t) t -> ('height, 'depth Peano.t) t * ('height, 'depth Peano.t) t  = 
    function
    | (x :: _ as xs) -> 
      if x then
        (sibling xs, xs) else (xs, sibling xs)

  let to_comparable : ('height, 'depth) t -> int * bool list =
    fun t -> 
    (Bounded_Peano.to_int @@ Bounded_vector.get_peano t, Bounded_vector.to_list t)

  module E = struct

    module T = struct
      type t =
          E : ('height, 'depth) T.t -> t

      let compare = Comparable.lift [%compare: (int * bool list)] ~f:(fun (E t) -> to_comparable t )

      let sexp_of_t : type height depth. t -> Sexp.t = fun ( E xs) -> sexp_of_t xs

      let t_of_sexp _ = failwith "Need to implement"
    end

    include T

    include Comparable.Make(T)

  end

end

module OneToThree = struct
  type 'a t = 
    | One : (Peano.z Peano.t)t
    | Two : (Peano.z Peano.t Peano.t)t
    | Three : (Peano.z Peano.t Peano.t Peano.t)t

  module Vector = struct
    type nonrec ('elem, 'height, 'depth) t =
      ('elem, 'height, 'depth) Bounded_vector.t * 'depth t

    let x: ('elem, 'height, 'depth) t = (Bounded_vector.([1; 2; 3]), (Three))
  end

end

module Public_key = struct
  type t = String.t [@@deriving hash, sexp, bin_io]
end

module Account = struct
  type t = {
    public_key : Public_key.t;
    balance : int
  } [@@deriving hash, bin_io]
end

module Hash = struct
  type t = int [@@deriving hash, bin_io, sexp, compare]

  let merge t1 t2 = Int.hash_fold_t
end


let transform_bin (type a b) (a_to_b: a -> b) (b_to_a : b -> a) (bin_type : a Bin_prot.Type_class.t) : b Bin_prot.Type_class.t =
  let module M : Bin_prot.Binable.S with type t := b  = Bin_prot.Utils.Make_binable(struct 
      type t = b 

      let to_binable = b_to_a

      let of_binable = a_to_b

      module Binable = struct

        type t = a

        let bin_shape_t = bin_type.shape

        let bin_size_t = bin_type.writer.size

        let bin_write_t = bin_type.writer.write

        let bin_read_t = bin_type.reader.read

        let __bin_read_t__ = bin_type.reader.vtag_read

      end
    end) in
  M.bin_t


module Location = struct

  module T = struct
    type ('typ, 'depth) t =
      | Account : (z, Max_height.t) Merkle_address.t -> (Account.t, Max_height.t) t
      | Hash : ('height, 'depth) Merkle_address.t -> (Hash.t, 'depth) t

  end


  include T

  let account_bin_t :  (Account.t, Max_height.t) t Bin_prot.Type_class.t =
    let bounded_vector = Bounded_vector.bin_t Bounded_Peano.max_value bin_bool in
    transform_bin (fun location -> Account location) (fun (Account location) -> location)
      bounded_vector

  module E = struct
    type t = 
        E : (Hash.t, 'depth) T.t -> t
  end
end

module Bigstring = struct
  module T = struct

    include Bigstring
    let hash, hash_fold_t = Bigstring.(hash_t_frozen, hash_fold_t_frozen)

  end
  include T
  include Hashable.Make (T)
end

module Database_lookup = struct
  type _ t = 
    | Account_location : Public_key.t -> ((Account.t, Max_height.t) Location.t) t
    | Account : (Account.t, Max_height.t) Location.t -> Account.t t
    | Hash : (Hash.t, 'depth) Location.t -> Hash.t t
    | Last_allocated : ((Account.t, Max_height.t) Location.t) t

  module E = struct
    type with_value =
        With_value : 'a t * 'a -> with_value
  end

  let value_bin_t : type a. a t -> a Bin_prot.Type_class.t = function
    | Account (Account _) -> 
      Account.bin_t
    | Hash _ -> 
      Hash.bin_t
    | Account_location _->
      Location.account_bin_t
    | Last_allocated -> 
      Location.account_bin_t

  let prefix_bigstring prefix src =
    let src_len = Bigstring.length src in
    let dst = Bigstring.create (src_len + 1) in
    Bigstring.set dst 0 (Char.of_int_exn prefix) ;
    Bigstring.blit ~src ~src_pos:0 ~dst ~dst_pos:1 ~len:src_len ;
    dst

  let serialize_key : type a. a t -> Bigstring.t = function
    | Account (Account merkle_path)  ->
      prefix_bigstring (0xfe) (Bigstring.of_string @@ Merkle_address.serialize merkle_path)
    | Account_location public_key -> 
      prefix_bigstring (0xff) (Bigstring.of_string public_key)
    | Hash (Hash merkle_path) ->
      prefix_bigstring (Bounded_vector.length merkle_path) (Bigstring.of_string @@ Merkle_address.serialize merkle_path)
    | Last_allocated ->
      prefix_bigstring (0xff) (Bigstring.create 0)


  let serialize_value : type a. a t -> a -> Bigstring.t = 
    fun key value -> 
    let value_bin_t = value_bin_t key in
    Bin_prot.Utils.bin_dump value_bin_t.writer value
end 

type t = Bigstring.t Bigstring.Table.t

let get : type a. t -> a Database_lookup.t -> a option =
  fun t key -> 
  let open Option.Let_syntax in
  let serialized_key = Database_lookup.serialize_key key in
  let%map serialized_value = Bigstring.Table.find t serialized_key in
  let value_bin_t = Database_lookup.value_bin_t key in
  value_bin_t.reader.read serialized_value ~pos_ref:(ref 0)

let set_batch : t -> Database_lookup.E.with_value list -> unit =
  fun t ->
  List.iter ~f:(fun (Database_lookup.E.With_value (key, value)) -> 
      let serialized_key = Database_lookup.serialize_key key  in
      let serialized_value = Database_lookup.serialize_value key value in
      Hashtbl.set t ~key:serialized_key ~data:serialized_value
    )

type cache = Hash.t Merkle_address.E.Map.t
type queue = (Hash.t * Merkle_address.E.t) Fqueue.t

let compute_hashes t (hashes_to_compute: (Hash.t * (Account.t, Max_height.t) Location.t) list) =
  let iter_step : t -> (cache * queue)  -> ((Hash.t * Merkle_address.E.t), (cache * queue)) Sequence.Step.t =
    fun t (map,  queue) -> 
      match Fqueue.dequeue queue with
      | None -> Done
      | Some ((hash, E (merkle_path) ), tail) ->
        match merkle_path with
        | Bounded_vector.[] -> Yield ((hash, E (merkle_path) ), (map, tail) )
        | (x::xs) as location -> 
          let sibling = Merkle_address.sibling location in
          let (sibling_hash, new_map) = match Map.find map (E sibling) with
            | None -> 
              let sibling_hash = 
                match (get t (Database_lookup.Hash (Location.Hash sibling ) ) ) with
                | Some hash -> hash
                | None -> default_hash sibling  in
              let new_map = Map.set map ~key:(Merkle_address.E.E sibling) ~data:sibling_hash  in
              (sibling_hash, new_map)
            | Some sibling_hash -> 
              (sibling_hash, map)
          in
          let parent_hash = if x then  (Hash.merge hash sibling_hash) else (Hash.merge sibling_hash hash) in
          Yield ((hash, E (merkle_path) ), (Map.set new_map ~key:(Merkle_address.E.E xs) ~data:sibling_hash, Fqueue.enqueue tail (parent_hash, Merkle_address.E.E xs) ) ) 
  in
  let map_input = List.map hashes_to_compute ~f:
      (fun (hash, Location.Account merkle_address) ->  
         (Merkle_address.E.E merkle_address, hash)
      ) in
  let queue_input = List.map hashes_to_compute ~f:(fun (hash, Location.Account merkle_address) ->  (hash, Merkle_address.E.E merkle_address)) in
  let init = (Merkle_address.E.Map.of_alist_exn map_input, Fqueue.of_list queue_input )in
  let results = Sequence.to_list @@ Sequence.unfold_step ~init ~f:(iter_step t) in
  List.map results ~f:(fun (hash, Merkle_address.E.E merkle_path) -> 
      Location.(  Database_lookup.E.With_value (Database_lookup.Hash (Location.Hash merkle_path), hash)   ) )


let merkle_root : t -> Hash.t =
  fun mdb -> 
  match  (get mdb (Database_lookup.Hash (Location.Hash Bounded_vector.[] ) ) ) with
  | None -> default_hash Bounded_vector.[] 
  | Some hash -> hash

let rec recompute_merkle_path : t -> (Hash.t * Merkle_address.E.t) list -> (Hash.t * Merkle_address.E.t) -> (Hash.t * Merkle_address.E.t) list  =
  fun mdb acc -> 
  function 
  | Bounded_vector.(hash, E []) -> (hash, E []) :: acc
  | (hash, E ((x::_) as path) ) -> 
    let sibling = Merkle_address.sibling path in
    let sibling_hash = 
      match (get mdb (Database_lookup.Hash (Location.Hash sibling ) ) ) with
      | Some hash -> hash
      | None -> default_hash sibling  in
    let parent_hash = if not x then
        merge hash sibling_hash else merge sibling_hash hash in
    let parent = Merkle_address.parent path in
    recompute_merkle_path mdb ((hash, E path) :: acc) (parent_hash, E parent)


let allocate_account : t -> Account.t -> unit = 
  fun t account -> 
  let (Account last_allocated) : (Account.t, Max_height.t) Location.t = Option.value_map (get t Database_lookup.Last_allocated) ~default:(Merkle_address.zero)
      ~f:(fun (Location.Account merkle_address) -> Merkle_address.next merkle_address )
  in
  let account_hash = Account.hash account in
  let computed_hashes = recompute_merkle_path t [] (account_hash, E last_allocated) in
  let modified_changes = 
    Database_lookup.E.With_value (Account (Account last_allocated), account) ::
    Database_lookup.E.With_value (Account_location (account.public_key), (Account last_allocated)) ::
    computed_hashes
  in
  set_batch t modified_changes



let set_accounts : t -> (Account.t * (Account.t, Max_height.t) Location.t) list -> unit = 
  fun t accounts_with_locations -> 
  let hashes_to_compute = List.map accounts_with_locations ~f:(fun (account, location) ->  (Account.hash account, location) ) in
  let computed_hashes = compute_hashes t hashes_to_compute in
  let last_allocated = Option.value_map (get t Database_lookup.Last_allocated) ~default:(Merkle_address.zero)
      ~f:(fun (Location.Account merkle_address) -> Merkle_address.next merkle_address )
  in
  let max_location = 
    List.fold ~init:last_allocated accounts_with_locations 
      ~f:(fun last_allocated (_, Account merkle_address)  -> Merkle_address.max last_allocated merkle_address  )
  in
  let modified_changes = 
    Database_lookup.E.With_value (Database_lookup.Last_allocated, max_location)::
    List.map accounts_with_locations ~f:(fun (account, location) -> Database_lookup.E.With_value (Account location, account)) @
    List.map accounts_with_locations ~f:(fun (account, location) -> Database_lookup.E.With_value (Account_location (account.public_key), location)) @
    computed_hashes
  in
  set_batch t modified_changes
