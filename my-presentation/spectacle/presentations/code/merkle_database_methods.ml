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