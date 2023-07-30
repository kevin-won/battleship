open Battleship

type input = string list

type command =
  | Add of ship_type
  | Remove of ship_type
  | Attack
  | Quit
  | Invalid

(* --------------------------------Add-Or-Remove-Ship: "Add
   Carrier"----------------------------------- *)

type add_or_remove_ship_outcome =
  | Add of ship_type
  | Remove of ship_type

(** [helper_valid_add_or_remove_ship_command] returns a) [Remove ship] if
    [remove] is [true] and [ship] is in [lst]. b) [Error CannotRemove] if
    [remove] is [true] and [ship] is not in [lst]. c) [Add ship] if [remove] is
    [false] and [ship] is in [lst]. d) [Error CannotAdd] if [remove] is [false]
    and [ship] is not in [lst]. *)
let helper_valid_add_or_remove_ship_command ship lst remove =
  if remove then if List.mem ship lst then Remove ship else raise CannotRemove
  else if List.mem ship lst then Add ship
  else raise CannotAdd

(** [remove_empty_strs] takes [acc] and prepends to it all elements in [lst_str]
    that are non-empty strings. *)
let rec remove_empty_strs lst_str acc =
  match lst_str with
  | [] -> acc
  | h :: t ->
      if h <> "" then remove_empty_strs t (h :: acc)
      else remove_empty_strs t acc

let print_error_msg msg =
  print_endline msg;
  print_endline ""

let print_exception_message (e : exn) =
  match e with
  | Battleship.Empty1 ->
      print_error_msg
        "Command is empty. The format is: <add/remove> <ship>. Try again."
  | Battleship.Malformed1 ->
      print_error_msg
        "Command is malformed. The format is: <add/remove> <ship>. Try again."
  | Battleship.CannotAdd ->
      print_error_msg "That ship is already on the board. Try again. "
  | Battleship.CannotRemove ->
      print_error_msg "That ship is already not on the board. Try again. "
  | Battleship.Empty2 ->
      print_error_msg
        "Command is empty. The format is: <orientation> 'at' <start-location>. \
         Try again."
  | Battleship.Malformed2 ->
      print_error_msg
        "Command is malformed. The format is: <orientation> 'at' \
         <start-location>. Try again."
  | Battleship.NotANumber ->
      print_error_msg
        "Start location is not written as two integers. Try again."
  | Battleship.OutOfBounds ->
      print_error_msg
        "Start location coordinates is out of bounds; they must be between 0 \
         and 9, inclusive. Try again."
  | Battleship.ShipDoesNotFit ->
      print_error_msg
        "Ship at given start location facing given orientation spills outside \
         board. Try again."
  | Battleship.ShipOverlaps ->
      print_error_msg
        "Ship at given start location facing given orientation collides with \
         another ship on the board. Try again."
  | Battleship.Empty3 ->
      print_error_msg "Command is empty. The format is: (row,col). Try again."
  | Battleship.Malformed3 ->
      print_error_msg
        "Command is malformed. The format is: (row,col). Try again."
  | _ -> print_error_msg "dd"

let valid_add_or_remove_ship_command command ships_to_add ships_added =
  if String.length command = 0 then raise Empty1
  else
    let lowercase_command = String.lowercase_ascii command in
    let lst_command = String.split_on_char ' ' lowercase_command in
    let final_lst_command = List.rev (remove_empty_strs lst_command []) in
    if List.length final_lst_command <> 2 then raise Malformed1
    else if List.nth final_lst_command 0 = "remove" then
      match List.nth final_lst_command 1 with
      | "carrier" ->
          helper_valid_add_or_remove_ship_command Carrier ships_added true
      | "battleship" ->
          helper_valid_add_or_remove_ship_command Battleship ships_added true
      | "cruiser" ->
          helper_valid_add_or_remove_ship_command Cruiser ships_added true
      | "submarine" ->
          helper_valid_add_or_remove_ship_command Submarine ships_added true
      | "destroyer" ->
          helper_valid_add_or_remove_ship_command Destroyer ships_added true
      | _ -> raise Malformed1
    else if List.nth final_lst_command 0 = "add" then
      match List.nth final_lst_command 1 with
      | "carrier" ->
          helper_valid_add_or_remove_ship_command Carrier ships_to_add false
      | "battleship" ->
          helper_valid_add_or_remove_ship_command Battleship ships_to_add false
      | "cruiser" ->
          helper_valid_add_or_remove_ship_command Cruiser ships_to_add false
      | "submarine" ->
          helper_valid_add_or_remove_ship_command Submarine ships_to_add false
      | "destroyer" ->
          helper_valid_add_or_remove_ship_command Destroyer ships_to_add false
      | _ -> raise Malformed1
    else raise Malformed1

(* --------------------------------Location-and-Direction: "Left at
   (1,2)"----------------------------------- *)

type orientation_and_location_outcome = orientation * int * int

(** [is_int] returns [true] if [s] is an int, [false] otherwise. *)
let is_int s =
  try
    let _ = int_of_string s in
    true
  with Failure _ -> false

let print_list lst = List.iter (fun x -> print_string x) lst

let extract_coordinates str malformed =
  let split_string_lst = String.split_on_char ',' str in
  let final_lst_str = List.rev (remove_empty_strs split_string_lst []) in
  if List.length final_lst_str <> 2 then raise malformed
  else
    let left_of_comma = List.nth final_lst_str 0 in
    if String.length left_of_comma = 0 then raise malformed
    else if String.sub left_of_comma 0 1 <> "(" then raise malformed
    else
      let right_of_comma = List.nth final_lst_str 1 in
      if String.length right_of_comma = 0 then raise malformed
      else if
        String.sub right_of_comma (String.length right_of_comma - 1) 1 <> ")"
      then raise malformed
      else
        let left_num_str =
          String.sub left_of_comma 1 (String.length left_of_comma - 1)
        in
        let right_num_str =
          String.sub right_of_comma 0 (String.length right_of_comma - 1)
        in
        if (not (is_int left_num_str)) || not (is_int right_num_str) then
          raise NotANumber
        else
          let left_num_int = left_num_str |> int_of_string in
          let right_num_int = right_num_str |> int_of_string in
          if
            left_num_int < 0 || left_num_int > 9 || right_num_int < 0
            || right_num_int > 9
          then raise OutOfBounds
          else (left_num_int, right_num_int)

let valid_attack_command command =
  let trimmed_command = String.trim command in
  if String.length trimmed_command = 0 then raise Empty3
  else extract_coordinates trimmed_command Malformed3

(** [valid_orientation_and_location_command_helper] carries the work of
    [valid_orientation_and_location_command], returning
    [Valid (orientation, row, col)] if [command] is valid or [Error e] if not. *)
let valid_orientation_and_location_command_helper str ship_type orientation
    ship_board =
  let coordinates = extract_coordinates str Malformed2 in
  let row = fst coordinates in
  let col = snd coordinates in
  let size = ship_size ship_type in
  if not (in_bounds row col size orientation) then raise ShipDoesNotFit
  else if ship_overlaps ship_board size row col orientation then
    raise ShipOverlaps
  else { ship_type; orientation; start_location = (row, col) }

let valid_orientation_and_location_command command ship_type ship_board =
  if String.length command = 0 then raise Empty2
  else
    let lowercase_command = String.lowercase_ascii command in
    let lst_command = String.split_on_char ' ' lowercase_command in
    let final_lst_command = List.rev (remove_empty_strs lst_command []) in
    if
      List.length final_lst_command <> 3 || List.nth final_lst_command 1 <> "at"
    then raise Malformed2
    else
      match List.nth final_lst_command 0 with
      | "left" ->
          valid_orientation_and_location_command_helper
            (List.nth final_lst_command 2)
            ship_type Left ship_board
      | "up" ->
          valid_orientation_and_location_command_helper
            (List.nth final_lst_command 2)
            ship_type Up ship_board
      | "right" ->
          valid_orientation_and_location_command_helper
            (List.nth final_lst_command 2)
            ship_type Right ship_board
      | "down" ->
          valid_orientation_and_location_command_helper
            (List.nth final_lst_command 2)
            ship_type Down ship_board
      | _ -> raise Malformed2

(* let parse board str = let str_lst = String.split_on_char ' ' str in if
   List.length str_lst == 0 then raise Empty else if List.nth str_lst 0 = "Add"
   && List.length str_lst = 6 && List.nth str_lst 2 = "facing" && List.nth
   str_lst 4 = "at" then let ship_type = extract_ship_type (List.nth str_lst 1)
   in let orientation = extract_orientation (List.nth str_lst 3) in let
   start_location = extract_start_location (List.nth str_lst 5) in let ship =
   {ship_type=ship_type; orientation=orientation; start_location=start_location}
   in add_ship board ship; else if List.nth str_lst 0 = "Attack" && List.length
   str_lst = 3 && List.nth str_lst 1 = "at" then let start_location =
   extract_start_location (List.nth str_lst 2) in let row = fst start_location
   in let col = snd start_location in attack board row col; (* fix this *) else
   raise Malformed *)
