(* Generic could be either serialized public keys or the index of the last allocated location *)
type t =
  | Generic of Bigstring.t
  | Account of Addr.t
  | Hash of Addr.t

let is_generic = function Generic _ -> true | _ -> false

let is_account = function Account _ -> true | _ -> false

let is_hash = function Hash _ -> true | _ -> false

let height : t -> int = function
  | Generic _ ->
    raise (Invalid_argument "height: generic location has no height")
  | Account _ ->
    0
  | Hash path ->
    Addr.height path

let parent : t -> t = function
  | Generic _ ->
    raise (Invalid_argument "parent: generic locations have no parent")
  | Account _ ->
    raise (Invalid_argument "parent: account locations have no parent")
  | Hash path ->
    assert (Addr.depth path > 0) ;
    Hash (Addr.parent_exn path)

let next : t -> t Option.t = function
  | Generic _ ->
    raise
      (Invalid_argument "next: generic locations have no next location")
  | Account path ->
    Addr.next path |> Option.map ~f:(fun next -> Account next)
  | Hash path ->
    Addr.next path |> Option.map ~f:(fun next -> Hash next)