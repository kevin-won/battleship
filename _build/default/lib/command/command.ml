open Battleship

(* -----------------Add-Or-Remove-Ship: "Add
   Carrier"------------------------- *)

type add_or_remove_ship_command =
  | Add of ship_type
  | Remove of ship_type

(** [helper_valid_add_or_remove_ship_command] (a) returns [Remove ship_type] if
    [remove] is [true] and [ship_type] is in [lst]; (b) raises the
    [CannotRemove] exception if [remove] is [true] and [ship_type] is not in
    [lst]; (c) returns [Add ship_type] if [remove] is [false] and [ship_type] is
    in [lst]; and (d) raises the [CannotAdd exception] if [remove] is [false]
    and [ship_type] is not in [lst]. *)
let helper_valid_add_or_remove_ship_command ship_type lst remove =
  if remove then
    if List.mem ship_type lst then Remove ship_type else raise CannotRemove
  else if List.mem ship_type lst then Add ship_type
  else raise CannotAdd

(** [remove_empty_strs] takes [acc] and prepends to it all elements in [lst_str]
    that are non-empty strings. *)
let rec remove_empty_strs lst_str acc =
  match lst_str with
  | [] -> acc
  | h :: t ->
      if h <> "" then remove_empty_strs t (h :: acc)
      else remove_empty_strs t acc

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

type orientation_and_location_command = orientation * position

(** [is_int] returns [true] if [s] is an int, [false] otherwise. *)
let is_int s =
  try
    let _ = int_of_string s in
    true
  with Failure _ -> false

(** [extract_coordinates] returns the coordinates expressed in
    [coordinates_command] if [coordinates_command] is valid. A valid
    [coordinates_command] is defined as a single word with no spaces in the form
    "(row,col)", where row and col are integers from 0 to 9, inclusive. If
    command is not valid, an exception is raised.

    (a) [NotANumber] is raised if the command contains coordinates, but the
    coordinates are not written as integers; (b) [OutOfBounds] is raised if the
    coordinates given are not from 0 to 9, inclusive; (c) [malformed] is raised
    in all other cases, and [malformed] for our game will be either [Malformed2]
    when used to add ships, or [Malformed3] when used to attack a ship. *)
let extract_coordinates coordinates_command malformed =
  let split_string_lst = String.split_on_char ',' coordinates_command in
  if List.length split_string_lst <> 2 then raise malformed
  else
    let left_of_comma = List.nth split_string_lst 0 in
    if String.length left_of_comma = 0 then raise malformed
    else if String.sub left_of_comma 0 1 <> "(" then raise malformed
    else
      let right_of_comma = List.nth split_string_lst 1 in
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

(** [valid_orientation_and_location_command_helper] returns the new ship to be
    added to ship board if [coordinates_command] is valid, and raises an
    exception if [coordinates_command] is not valid. A valid
    [coordinates_command] is defined in [extract_coordinates].

    Since this function calls [extract_coordinates], any exception thrown in
    [extract_coordinates] will be thrown here. Additional exceptions that might
    be thrown in this function are (a) [ShipDoesNotFit] if the ship at its start
    location facing [orientation] spills outside the board, and (b)
    [ShipOverlaps] if the ship at its start location facing [orientation]
    collides with another ship on [ship_board]. *)
let valid_orientation_and_location_command_helper coordinates_command ship_type
    orientation ship_board =
  let start_location = extract_coordinates coordinates_command Malformed2 in
  let ship = { ship_type; orientation; start_location } in
  if not (ship_fits ship) then raise ShipDoesNotFit
  else if ship_overlaps ship_board ship then raise ShipOverlaps
  else ship

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

(*-------------------- ATTACK ------------------------------------------*)

let valid_attack_command coordinates_command attacked_board =
  let trimmed_coordinates_command = String.trim coordinates_command in
  if String.length trimmed_coordinates_command = 0 then raise Empty3
  else
    let coordinates =
      extract_coordinates trimmed_coordinates_command Malformed3
    in
    let block = get_value_at attacked_board coordinates in
    if block.attacked <> Not then raise AlreadyAttacked else coordinates

(** -------------------------UTILITY-------------------------*)

let print_error_msg msg =
  print_endline msg;
  print_endline ""

let print_exception_message e =
  match e with
  | Empty1 ->
      print_error_msg
        "Command is empty. The format is: <add/remove> <ship>. Try again."
  | Malformed1 ->
      print_error_msg
        "Command is malformed. The format is: <add/remove> <ship>. Try again."
  | CannotAdd ->
      print_error_msg "That ship is already on the board. Try again. "
  | CannotRemove ->
      print_error_msg "That ship is already not on the board. Try again. "
  | Empty2 ->
      print_error_msg
        "Command is empty. The format is: <orientation> 'at' <start-location>. \
         Try again."
  | Malformed2 ->
      print_error_msg
        "Command is malformed. The format is: <orientation> 'at' \
         <start-location>. Try again."
  | NotANumber ->
      print_error_msg
        "Start location is not written as two integers. Try again."
  | OutOfBounds ->
      print_error_msg
        "Start location coordinates is out of bounds; they must be between 0 \
         and 9, inclusive. Try again."
  | ShipDoesNotFit ->
      print_error_msg
        "Ship at given start location facing given orientation spills outside \
         board. Try again."
  | ShipOverlaps ->
      print_error_msg
        "Ship at given start location facing given orientation collides with \
         another ship on the board. Try again."
  | Empty3 ->
      print_error_msg "Command is empty. The format is: (row,col). Try again."
  | Malformed3 ->
      print_error_msg
        "Command is malformed. The format is: (row,col). Try again."
  | AlreadyAttacked -> print_error_msg "You already attacked there. Try again."
  | _ -> raise ThisWillNeverHappen
