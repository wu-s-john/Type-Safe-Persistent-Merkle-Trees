let get : type a. t -> a Database_lookup.t -> a option =
  fun t key -> 
  let open Option.Let_syntax in
  let serialized_key = Database_lookup.serialize_key key in
  let%map serialized_value = Bigstring.Table.find t serialized_key in
  let value_bin_t = Database_lookup.value_bin_t key in
  value_bin_t.reader.read serialized_value ~pos_ref:(ref 0)


let get_account_path_action = Option.value_exn (get t (Database_lookup.Account_location "0x1337"))

let get_account_path_action = get t (Database_lookup.Account get_account_path_action)

let set_batch : t -> Database_lookup.E.with_value list -> unit =
  fun t ->
  List.iter ~f:(fun (Database_lookup.E.With_value (key, value)) -> 
      let serialized_key = Database_lookup.serialize_key key  in
      let serialized_value = Database_lookup.serialize_value key value in
      Hashtbl.set t ~key:serialized_key ~data:serialized_value
    )