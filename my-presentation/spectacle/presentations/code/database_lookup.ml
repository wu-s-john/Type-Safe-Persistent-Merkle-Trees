module Database_lookup = struct
  type _ t = 
    | Account_location : Public_key.t -> ((Account.t, Max_height.t) Location.t) t
    | Account : (Account.t, Max_height.t) Location.t -> Account.t t
    | Hash : (Hash.t, 'depth) Location.t -> Hash.t t
    | Last_allocated : ((Account.t, Max_height.t) Location.t) t

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