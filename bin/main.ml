open Battleship 
open Command
 
type person = {
  my_board : board;
}   

type state = {
  player1 : person 
}

 
let ship_lst_to_str lst = 
  if List.length lst = 0 then "[]" else 
  let str_ships = List.fold_left (fun acc ship -> acc ^ Battleship.ship_name ship ^ ", ") "[" lst in 
  let truncated_str_ships = String.sub str_ships 0 (String.length str_ships - 2) in 
  truncated_str_ships ^ "]"

 
let rec initialize_board ship_board ships_to_add ships_added = 
  let str_ships_to_add = ship_lst_to_str ships_to_add in
  let str_ships_added = ship_lst_to_str ships_added in 
  print_endline ("Ships to add: " ^ str_ships_to_add);
  print_endline ("Ships to remove: " ^  str_ships_added);
  print_endline "";
  print_ship_board ship_board;
  print_endline ""; 
  print_string ("What ship would you like to add or remove? ");
  let add_or_remove_ship_command = read_line () in 
  match valid_add_or_remove_ship_command add_or_remove_ship_command ships_to_add ships_added with 
  | Error error -> (match error with 
      | Empty -> print_endline "Command is empty. The format is: <add/remove> <ship>. Try again.";
        print_endline "";
        initialize_board ship_board ships_to_add ships_added
      | Malformed -> print_endline "Command is malformed. The format is: <add/remove> <ship>. Try again.";
        print_endline "";
        initialize_board ship_board ships_to_add ships_added
      | CannotAdd -> print_endline "That ship is already on the board. Try again. ";
        print_endline "";
        initialize_board ship_board ships_to_add ships_added
      | CannotRemove -> print_endline "That ship is already not on the board. Try again. ";
        print_endline "";
        initialize_board ship_board ships_to_add ships_added
      ) 
  | Remove ship_type ->  
      let ship_board = remove_ship ship_board ship_type in 
      let new_ships_to_add = ship_type :: ships_to_add in 
      let new_ships_added = List.filter (fun s -> s <> ship_type) ships_added in
      initialize_board ship_board new_ships_to_add new_ships_added
  | Add ship_type -> 
      print_string ("What's the start location and direction? ");
      let location_and_direction_command = read_line () in  
        (match valid_location_and_direction_command location_and_direction_command with 
        | Error e -> print_endline "Bad";
        | Valid (orientation, row, col) -> 
          let ship = {ship_type=ship_type; orientation=orientation; start_location=(row,col)} in 
          let ship_board = add_ship ship_board ship in
        let new_ships_added = ship_type :: ships_added in 
        let new_ships_to_add = List.filter (fun s -> s <> ship_type) ships_to_add in
        initialize_board ship_board new_ships_to_add new_ships_added)

let initialize_player () =
  let init_board = List.init (game_dimension) (fun _ -> 
    List.init (game_dimension) (fun _ -> {occupation=Unoccupied; attacked=Not})) in 
  let ships_to_add = [Carrier; Battleship; Cruiser; Submarine; Destroyer] in
  let ship_board = initialize_board init_board ships_to_add [] in
  let player1 = {my_board=ship_board} in 
  player1

let initialize_game_state () = 
  print_endline "Welcome to Battleship!";
  print_string "Player 1, enter your name: ";
  let player1_name = read_line () in
  print_string "Player 2, enter your name: ";
  let player2_name = read_line () in 
  print_endline "";
  let personalized_welcome = "Welcome, " ^ player1_name ^ " and " ^ player2_name ^ ". " in 
  print_endline (personalized_welcome ^ "Let’s add some ships, " ^ player1_name ^ " goes first!");
  print_endline "";
  let player1 = initialize_player () in
  let state = {player1=player1} in 
  state

(* let render_board game_state =

let rec game_loop game_state = 
  render_board game_state;

  let move = read_line () in 
  let new_game_state = process_move game_state move in 
  new_game_state  *)

let main () =
  let game_state = initialize_game_state () in
  print_ship_board game_state.player1.my_board

let _ = main ()