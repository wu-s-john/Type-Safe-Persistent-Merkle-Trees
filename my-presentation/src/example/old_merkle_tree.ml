
type t = Kvdb.t (* The keys and values are big strings *)

let get mdb location =
  assert (Location.is_account location) ;
  let raw_account = get_raw mdb location in
  deserialize_account raw_account

module Account_location = struct
  let get_generic mdb location =
    assert (Location.is_generic location) ;
    get_raw mdb location

  (* encodes a key as a location used as a database key, so we can find the
     account location associated with that key *)
  let build_location (key: Public_key.t) : string  =
    Location.build_generic
      (Bigstring.of_string ("$" ^ Format.sprintf !"%{sexp: Key.t}" key))

  let get mdb key =
    (* Unsafe *)
    match get_generic mdb (build_location key) with
    | None ->
      Error Db_error.Account_location_not_found
    | Some location_bin ->
      let result =
        Location.parse location_bin
        |> Result.map_error ~f:(fun () -> Db_error.Malformed_database)
      in
      result

  let increment_last_account_location mdb =
    let location = last_location_key () in
    match get_generic mdb location with
    | None ->
      let first_location =
        Location.Account
          ( Addr.of_directions
            @@ List.init Depth.depth ~f:(fun _ -> Direction.Left) )
      in
      set_raw mdb location (Location.serialize first_location) ;
      Result.return first_location
    | Some prev_location -> (
        match Location.parse prev_location with
        | Error () ->
          Error Db_error.Malformed_database
        | Ok prev_account_location ->
          Location.next prev_account_location
          |> Result.of_option ~error:Db_error.Out_of_leaves
          |> Result.map ~f:(fun next_account_location ->
              set_raw mdb location
                (Location.serialize next_account_location) ;
              next_account_location ) )

  let allocate mdb key =
    let location_result = increment_last_account_location mdb in
    Result.map location_result ~f:(fun location ->
        set mdb key location ; location )
end

let get_or_create_account mdb key account =
  match Account_location.get mdb key with
  | Error Account_location_not_found -> (
      match Account_location.allocate mdb key with
      | Ok location ->
        set mdb location account ;
        Ok (`Added, location)
      | Error err ->
        Error (Error.create "get_or_create_account" err Db_error.sexp_of_t) )
  | Error err ->
    Error (Error.create "get_or_create_account" err Db_error.sexp_of_t)
  | Ok location ->
    Ok (`Existed, location)



let set mdb location account =
  set_bin mdb location Account.bin_size_t Account.bin_write_t account ;
  set_hash mdb
    (Location.Hash (Location.to_path_exn location))
    (Hash.hash_account account)