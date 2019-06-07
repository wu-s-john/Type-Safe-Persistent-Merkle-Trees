// Import React
import React from 'react';

// Import Spectacle Core tags
import {
  BlockQuote,
  Cite,
  Deck,
  Heading,
  ListItem,
  List,
  Quote,
  Slide,
  Text,
  CodePane
} from 'spectacle';

// Import theme
import createTheme from 'spectacle/lib/themes/default';

require('prismjs/components/prism-ocaml');
require('prismjs/components/prism-reason');

// Require CSS
require('normalize.css');

const theme = createTheme(
  {
    primary: 'white',
    secondary: '#1F2022',
    tertiary: '#03A9FC',
    quaternary: '#CECECE',
  },
  {
    primary: 'Montserrat',
    secondary: 'Helvetica',
  }
);

const adt = `
type 'a option = 
    Some of 'a
  | None

module Either = struct
  type ('a, 'b) t = 
      First of 'a |
      Second of 'b
end

module Peano = struct
  type t = Z | S of t
  
  let two : t = S (S Z)
end

module Tree = struct
  type 'a t = 
    | Leaf 
    | Node of 'a t * 'a * 'a t
end
`

const adt_example = `


let my_tree = 
  let open Tree in
  Node ((Node (Leaf, 1, Node (Leaf, 3, Leaf)) ), 2, Leaf)

(**
      2
    /   \\
   1
     \\
       3
*)

let root : 'a t -> 'a option = 
  function 
  | Node (_, x, _) -> Some x
  | Leaf -> None

let value = root my_tree (* Some 2 *) 
`

const gadt = `
module Peano = struct
  type z = Zero

  type 'a t =   
    | Succ : 'a  -> ('a t)
end

let zero : Peano.z = 
  Peano.Z

let one : Peano.z Peano.t = 
  let open Peano in
  S zero

let two : Peano.z Peano.t Peano.t = 
  let open Peano in
  S one

let three : Peano.z Peano.t Peano.t Peano.t = 
  let open Peano in
  S two
`

const gadt_trees = `
module Balanced_tree = struct
  type ('a, 'depth) t =
    | Leaf : ('a, Peano.z) t
    | Node : ('a, 'n) t * 'a * ('a, 'n) t -> 
        ('a, 'n Peano.t) t
end

(* Compiling error: Not a balanced tree *)
(**
      2
    /   \\
   1
     \\
       3
*)
let my_tree = 
   let open Balanced_tree in
   Node ((Node (Leaf, 1, Node (Leaf, 3, Leaf)) ), 2, Leaf)    

let my_balanced_tree =
  let open Balanced_tree in
  let level1_left =
    Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 3, Leaf))
  in
  let level1_right =
    Node (Node (Leaf, 6, Leaf), 4, Node (Leaf, 7, Leaf))
  in
  Node (level1_left, 2, level1_right)

(**
          2
     /         \\
    1            4
   /   \\         /  \\
   0      3     6     7
*)

let get_value : ('a, 'n Peano.t) Balanced_tree.t -> 'a =
  function (Node (_, x, _) ) -> x
`

const gadt_vector = `
module Vector = struct

  type ('elem, 'size) t =
    | [] : ('elem, Peano.z) t
    | ( :: ) : 'elem * ('elem, 'n) t ->  ('elem, 'n Peano.t) t


  let three_elem_vector: (int, Peano.z Peano.t Peano.t Peano.t) t = [1; 2; 3]
end

module OneToThree = struct
  type 'a t = 
    | One : (Peano.z Peano.t)t
    | Two : (Peano.z Peano.t Peano.t)t
    | Three : (Peano.z Peano.t Peano.t Peano.t)t
end

module OneToThreeVector = struct
  type nonrec ('elem, 'n) t = ('elem, 'n) Vector.t * 'n OneToThree.t

  let three_elem_vector : (int, z Peano.t Peano.t Peano.t) t = 
    Vector.([1; 2; 3]), Three

  (* Compiling error *)
  let three_elem_vector_error : (int, z Peano.t Peano.t Peano.t) t = 
    Vector.([1; 2; 3]), Two
end
`

const old_merkle_address = `
type t = bool list

let max_height = 31

let parent : t -> t option = 
  function
  | [] -> None
  | _::xs -> Some xs

let child : t -> bool -> t option =
  fun address is_left ->
  if List.length address < max_height then 
    Some (is_left :: address)
  else 
    None

let sibling : t -> t option =
  function
  | [] -> None
  | x::xs -> Some ((not x) :: xs)
`

const old_merkle_location = `
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
`

const unit_tests = `
  let%test_unit "parent(child(node)) = node" =
    Quickcheck.test ~sexp_of:[%sexp_of: t * bool]
      (gen)
      ~f:(fun (merkle_path, direction) ->
        [%test_result: t] ~expect:merkle_path (Option.value_exn @@ parent (Option.value_exn @@ child merkle_path direction)) )
  
  ...
  
  let%test_unit "addr: sibling(sibling(addr)) = addr" =
    Quickcheck.test ~sexp_of:[%sexp_of: Direction.t list]
      (gen)
      ~f:(fun merkle_address ->
        [%test_result: t] ~expect:merkle_address (Option.value_exn @@ sibling (Option.value_exn sibling merkle_address) ) )
`

const bounded_vector = `
module Bounded_vector = struct
  type ('elem, _, _) t =
    | [] : ('elem, Max_height.t, Peano.z) t
    | ( :: ) : 'elem * ('elem, 'height Peano.t, 'depth) t ->  ('elem, 'height, 'depth Peano.t) t

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

  let bin_t : type height depth. (depth, height) Bounded_Peano.t -> ('a, ('a, height, depth) t) Bin_prot.Type_class.S1.t = 
    ...some type acrobatics
end
`

const gadt_merkle_address = `
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
  
  let serialize (t : ('height, 'depth) t) =  
    Bounded_vector.fold_left t ~init:"" ~f:(fun x acc -> if x then String.concat ~sep:"" ["1"; acc] else String.concat ~sep:"" ["0"; acc] )

  let to_comparable : ('height, 'depth) t -> int * bool list =
    fun t -> 
    (Bounded_Peano.to_int @@ Bounded_vector.get_peano t, Bounded_vector.to_list t)

  module E = struct

    module T = struct
      type t =
          E : ('height, 'depth) T.t -> t

      let compare = Comparable.lift [%compare: (int * bool list)] ~f:(fun (E t) -> to_comparable t )
    end

    include T

    include Comparable.Make(T)

  end
end
`

const gadt_location = `
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
`

const database_lookup = `
module Database_lookup = struct
  type _ t = 
    | Account_location : Public_key.t -> ((Account.t, Max_height.t) Location.t) t
    | Account : (Account.t, Max_height.t) Location.t -> Account.t t
    | Hash : (Hash.t, 'depth) Location.t -> Hash.t t
    | Last_allocated : ((Account.t, Max_height.t) Location.t) t

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

type t = Bigstring.t Bigstring.Table.t (* Database type *)
end 
`

const get_set_database = `
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
`

const merkle_database_methods = `
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

`

export default class Presentation extends React.Component {
  render() {
    return (
      <Deck
        transition={['zoom', 'slide']}
        transitionDuration={500}
        theme={theme}
      >
        <Slide transition={['zoom']} bgColor="primary">
          <Heading size={4} caps lineHeight={1} textColor="secondary">
            Designing More Secure Cryptographic Datastructures:
          </Heading>
          <Heading size={4} caps lineHeight={1} textColor="secondary">
            A "More" Type-Safe Persistent Merkle Tree
          </Heading>

          <Text margin="10px 0 0" textColor="tertiary" size={1} bold>
            John Wu
          </Text>
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Heading size={5} textColor="secondary" caps>
            ADTs (Algebraic Data Types)
          </Heading>
          <Text textColor="tertiary" size={1} fit bold>
            A type representing the union of other types
          </Text>
          <CodePane source={adt} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Heading size={6} textColor="secondary" caps>
            Example
          </Heading>
          <CodePane source={adt_example} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="tertiary">
          <Heading size={6} textColor="primary" caps>
            What if we want to construct balanced trees in a type-safe manner?
          </Heading>
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Heading size={5} textColor="secondary" caps>
            GADTs (Generalized Algebraic Data Types)
          </Heading>
          <Text textColor="tertiary" size={7} fit bold>
            An ADT with type annotatations
          </Text>
          <CodePane source={gadt} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Heading size={5} textColor="secondary" caps>
            GADT Balanced Trees
          </Heading>
          <CodePane source={gadt_trees} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Heading size={5} textColor="secondary" caps>
            GADT Vectors
          </Heading>
          <CodePane source={gadt_vector} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Text textColor="tertiary" size={1} fit bold>
            Current Implementation of a Merkle Address
          </Text>
          <Heading size={5} textColor="secondary" caps>
            Too many options
          </Heading>
          <CodePane source={old_merkle_address} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Text textColor="tertiary" size={1} fit bold>
            Current Implementation of Database Keys
          </Text>
          <Heading size={5} textColor="secondary" caps>
            Too much unsafe code
          </Heading>
          <CodePane source={old_merkle_location} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Heading size={5} textColor="secondary" caps>
            Too many unit tests to ensure correctness of the system
          </Heading>
          <CodePane source={unit_tests} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="tertiary">
          <Heading size={6} textColor="primary" caps>
            Can we get safer and concise code?
          </Heading>
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Heading size={5} textColor="secondary" caps>
            GADT Bounded Vector
          </Heading>
          <CodePane source={bounded_vector} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Text textColor="tertiary" size={1} fit bold>
            GADT Merkle Address
          </Text>
          <Heading size={5} textColor="secondary" caps>
            No Options needed
          </Heading>
          <CodePane source={gadt_merkle_address} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Heading size={5} textColor="secondary" caps>
            Type Annotated Account and Hash Locations
          </Heading>
          <CodePane source={gadt_location} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Heading size={5} textColor="secondary" caps>
            GADT Key-Value Pairs
          </Heading>
          <CodePane source={database_lookup} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Heading size={5} textColor="secondary" caps>
            Database functions
          </Heading>
          <CodePane source={get_set_database} lang="ocaml" />
        </Slide>
        <Slide transition={['fade']} bgColor="primary" textColor="tertiary">
          <Heading size={5} textColor="secondary" caps>
            Merkle Tree Functions
          </Heading>
          <CodePane source={merkle_database_methods} lang="ocaml" />
        </Slide>
      </Deck>
    );
  }
}
