open Battleship

type input = string list

type command =
  | Add of input
  | Attack of input
  | Quit

exception Empty

exception Malformed

let extract_ship_type s = 
  match s with 
  | "Carrier" -> Carrier
  | "Battleship" -> Battleship
  | "Cruiser" -> Cruiser
  | "Submarine" -> Submarine
  | "Destroyer" -> Destroyer
  | _ -> raise Malformed

let extract_orientation s =
  match s with 
  | "Left" -> Left
  | "Right" -> Right
  | "Up" -> Up
  | "Down" -> Down
  | _ -> raise Malformed

let is_digit_in_bounds c = 
  match c with 
  | '0' .. '9' -> true 
  | _ -> false

let int_of_char c =
  match c with 
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | _ -> raise Malformed

let extract_start_location s = 
  if s.[0] <> '(' || s.[2] <> ',' || s.[4] <> ')' 
    || not (is_digit_in_bounds s.[1]) || not (is_digit_in_bounds s.[3]) then raise Malformed else
      let row = int_of_char s.[1] in 
      let col = int_of_char s.[3] in 
      (row, col)

let parse str =
  let str_lst = String.split_on_char ' ' str in 
  if List.length str_lst == 0 then raise Empty else 
    if List.nth str_lst 0 = "Add" && List.length str_lst = 6 &&
    List.nth str_lst 2 = "facing" && List.nth str_lst 4 = "at" then 
      let ship_type = extract_ship_type (List.nth str_lst 1) in 
      let orientation = extract_orientation (List.nth str_lst 3) in
      let start_location = extract_start_location (List.nth str_lst 5) in 
      let ship = {ship_type=ship_type; orientation=orientation; start_location=start_location} in 
      add_ship ship;
  else if List.nth str_lst 0 = "Attack" && List.length str_lst = 3 && List.nth str_lst 1 = "at" then
    let start_location = extract_start_location (List.nth str_lst 2) in 
    let row = fst start_location in
    let col = snd start_location in 
    attack row col;
  else raise Malformed