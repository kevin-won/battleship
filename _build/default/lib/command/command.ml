open Battleship  
 
type input = string list

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

let rec remove_empty_strs lst_str acc = 
  match lst_str with 
  | [] -> acc 
  | h :: t -> if h <> "" then remove_empty_strs t (h :: acc) else remove_empty_strs t acc

type command =
| Add of ship_type
| Remove of ship_type
| Attack 
| Quit 
| Invalid

type error1 =
| Empty
| Malformed
| CannotAdd
| CannotRemove

type add_or_remove_ship_command_one = 
| Add of ship_type
| Remove of ship_type
| Error of error1
 
let helper_valid_add_or_remove_ship_command ship lst remove =
  if remove then if List.mem ship lst then Remove ship else Error CannotRemove else
    if List.mem ship lst then Add ship else Error CannotAdd
 
let valid_add_or_remove_ship_command str ships_to_add ships_added : add_or_remove_ship_command_one = 
  if String.length str = 0 then Error Empty else 
  let lower_str = String.lowercase_ascii str in 
  let lst_str = String.split_on_char ' ' lower_str in 
  let final_lst_str = List.rev (remove_empty_strs lst_str []) in 
  if List.length final_lst_str <> 2 then Error Malformed else 
    if List.nth final_lst_str 0 = "remove" then 
      match List.nth final_lst_str 1 with
      | "carrier" -> helper_valid_add_or_remove_ship_command Carrier ships_added true
      | "battleship" -> helper_valid_add_or_remove_ship_command Battleship ships_added true
      | "cruiser" -> helper_valid_add_or_remove_ship_command Cruiser ships_added true
      | "submarine" -> helper_valid_add_or_remove_ship_command Submarine ships_added true
      | "destroyer" -> helper_valid_add_or_remove_ship_command Destroyer ships_added true
      | _ -> Error Malformed
  else if List.nth final_lst_str 0 = "add" then 
    match List.nth final_lst_str 1 with
    | "carrier" -> helper_valid_add_or_remove_ship_command Carrier ships_to_add false
    | "battleship" -> helper_valid_add_or_remove_ship_command Battleship ships_to_add false
    | "cruiser" -> helper_valid_add_or_remove_ship_command Cruiser ships_to_add false
    | "submarine" -> helper_valid_add_or_remove_ship_command Submarine ships_to_add false
    | "destroyer" -> helper_valid_add_or_remove_ship_command Destroyer ships_to_add false
    | _ -> Error Malformed
else Error Malformed

let is_int (s : string) : bool =
  try
    let _ = int_of_string s in 
    true
  with
  | Failure _ -> false


type error2 =
| Empty
| Malformed
| NotANumber
| OutOfBounds
| ShipDoesNotFit
| ShipOverlaps

type start_loc_and_direction_command_two = 
| Valid of orientation * int * int
| Error of error2


let in_bounds row col ship_size orientation = 
  match orientation with 
  | Left -> if col + 1 - ship_size < 0 then false else true
  | Right ->if col - 1 + ship_size > 9 then false else true
  | Up -> if row + 1 - ship_size < 0 then false else true
  | Down -> if row - 1 + ship_size > 9 then false else true


let extract_helper s ship_type orientation board = 
  let split_string_lst = String.split_on_char ',' s in 
  let final_lst_str = List.rev (remove_empty_strs split_string_lst []) in 
  if List.length final_lst_str <> 2 then Error Malformed 
  else 
    let left_of_comma = List.nth final_lst_str 0 in
    if String.sub left_of_comma 0 1 <> "(" then Error Malformed else 
      let right_of_comma = List.nth final_lst_str 1 in 
      if String.sub right_of_comma (String.length right_of_comma - 1) 1 <> ")"  then Error Malformed else
        let left_num_str = String.sub left_of_comma 1 (String.length left_of_comma - 1) in 
        let right_num_str = String.sub right_of_comma 1 (String.length right_of_comma - 1) in 
        if not (is_int left_num_str) || not (is_int right_num_str) then Error NotANumber else 
          let left_num_int = left_num_str |> int_of_string in 
          let right_num_int = right_num_str |> int_of_string in 
          if left_num_int < 0 || left_num_int > 9 || right_num_int < 0 || right_num_int > 9 then Error OutOfBounds else 
            let size = ship_size ship_type in
            if not (in_bounds left_num_int right_num_int size orientation) then Error ShipDoesNotFit else 
            if ship_overlaps board size left_num_int right_num_int orientation then Error ShipOverlaps else 
              Valid (orientation, left_num_int, right_num_int)

let valid_start_loc_and_direction_command str ship_type board = 
  if String.length str = 0 then Error Empty else 
  let lower_str = String.lowercase_ascii str in 
  let lst_str = String.split_on_char ' ' lower_str in 
  let final_lst_str = List.rev (remove_empty_strs lst_str []) in 
  if List.length final_lst_str <> 3 || List.nth final_lst_str 1 <> "at" then Error Malformed else
    match List.nth final_lst_str 0 with 
    | "left" -> extract_helper (List.nth final_lst_str 1) ship_type Left board
    | "up" -> extract_helper (List.nth final_lst_str 1) ship_type Up board
    | "right" -> extract_helper (List.nth final_lst_str 1) ship_type Right board
    | "down" -> extract_helper (List.nth final_lst_str 1) ship_type Down board
    | _ -> Error Malformed


let parse board str =
  let str_lst = String.split_on_char ' ' str in 
  if List.length str_lst == 0 then raise Empty else 
    if List.nth str_lst 0 = "Add" && List.length str_lst = 6 &&
    List.nth str_lst 2 = "facing" && List.nth str_lst 4 = "at" then 
      let ship_type = extract_ship_type (List.nth str_lst 1) in 
      let orientation = extract_orientation (List.nth str_lst 3) in
      let start_location = extract_start_location (List.nth str_lst 5) in 
      let ship = {ship_type=ship_type; orientation=orientation; start_location=start_location} in 
      add_ship board ship;
  else if List.nth str_lst 0 = "Attack" && List.length str_lst = 3 && List.nth str_lst 1 = "at" then
    let start_location = extract_start_location (List.nth str_lst 2) in 
    let row = fst start_location in
    let col = snd start_location in 
    attack board row col;
    (* fix this *)
  else raise Malformed