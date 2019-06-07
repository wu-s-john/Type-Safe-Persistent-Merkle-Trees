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