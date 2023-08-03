open Battleship
open Command
open Person
open State

(* let render_board game_state =

   let rec game_loop game_state = render_board game_state;

   let move = read_line () in let new_game_state = process_move game_state move
   in new_game_state *)

let clear_screen () = print_string "\027[2J\027[1;1H"

(* [bool] is [true] if player1 = my_player *)
let rec attack_loop my_player opponent_player bool =
  print_attacked_board my_player.attacked_board;
  print_endline "";
  print_ship_board my_player.ship_board;
  print_endline "";
  print_string (my_player.name ^ ", where would you like to attack? ");
  let attack_command = read_line () in
  try
    let coordinates =
      valid_attack_command attack_command my_player.attacked_board
    in
    let ( my_attacked_board,
          opponent_ship_board,
          new_opponent_ship_health,
          attack_outcome ) =
      attack my_player.attacked_board opponent_player.ship_board
        opponent_player.ship_health coordinates
    in
    let my_player = { my_player with attacked_board = my_attacked_board } in
    let opponent_player =
      {
        opponent_player with
        ship_board = opponent_ship_board;
        ship_health = new_opponent_ship_health;
      }
    in
    (match attack_outcome with
    | Miss -> print_endline "Miss!"
    | Hit -> print_endline "Hit!"
    | Sunk -> print_endline "Sunk!"
    | Not -> raise ThisWillNeverHappen);
    if game_over opponent_player.ship_health then (
      print_endline ("Game over! " ^ my_player.name ^ " won the game!");
      exit 0)
    else print_attacked_board my_attacked_board;
    print_endline "";
    print_string
      ("Click [return] to clear the screen and then pass the computer to "
     ^ opponent_player.name);
    let _ = read_line () in
    clear_screen ();
    print_string (opponent_player.name ^ ", click [return] to start your turn");
    let _ = read_line () in
    if bool then { player1 = my_player; player2 = opponent_player }
    else { player1 = opponent_player; player2 = my_player }
  with e ->
    print_exception_message e;
    attack_loop my_player opponent_player bool

let rec game_loop game_state =
  let state_after_player1 =
    attack_loop game_state.player1 game_state.player2 true
  in
  let state_after_player2 =
    attack_loop state_after_player1.player2 state_after_player1.player1 false
  in
  print_endline "";
  game_loop state_after_player2

(* -----------------------------------INITIALIZE
   GAME------------------------------- *)

let ship_lst_to_str lst =
  if List.length lst = 0 then "[]"
  else
    let str_ships =
      List.fold_left
        (fun acc ship -> acc ^ Battleship.ship_name ship ^ ", ")
        "[" lst
    in
    let truncated_str_ships =
      String.sub str_ships 0 (String.length str_ships - 2)
    in
    truncated_str_ships ^ "]"

let rec initialize_board ship_board ships_to_add ships_added =
  if List.length ships_to_add <> 0 then (
    let str_ships_to_add = ship_lst_to_str ships_to_add in
    let str_ships_added = ship_lst_to_str ships_added in
    print_endline ("Ships to add: " ^ str_ships_to_add);
    print_endline ("Ships to remove: " ^ str_ships_added);
    print_endline "";
    print_ship_board ship_board;
    print_endline "";
    print_string "What ship would you like to add or remove? ";
    let add_or_remove_ship_command = read_line () in
    try
      match
        valid_add_or_remove_ship_command add_or_remove_ship_command ships_to_add
          ships_added
      with
      | Remove ship_type ->
          let ship_board = remove_ship ship_board ship_type in
          let new_ships_to_add = ship_type :: ships_to_add in
          let new_ships_added =
            List.filter (fun s -> s <> ship_type) ships_added
          in
          initialize_board ship_board new_ships_to_add new_ships_added
      | Add ship_type -> (
          print_string "What's the orientation and start location? ";
          let orientation_and_location_command = read_line () in
          try
            let ship =
              valid_orientation_and_location_command
                orientation_and_location_command ship_type ship_board
            in
            let new_ship_board = add_ship ship_board ship in
            let new_ships_added = ship_type :: ships_added in
            let new_ships_to_add =
              List.filter (fun s -> s <> ship_type) ships_to_add
            in
            initialize_board new_ship_board new_ships_to_add new_ships_added
          with e ->
            print_exception_message e;
            initialize_board ship_board ships_to_add ships_added)
    with e ->
      print_exception_message e;
      initialize_board ship_board ships_to_add ships_added)
  else ship_board

let initialize_player name =
  let init_ship_board = Battleship.init_board in
  let init_attacked_board = Battleship.init_board in
  let init_ship_health = Battleship.init_ship_health in
  let init_ships_to_add = Battleship.init_ships_to_add in
  let ship_board = initialize_board init_ship_board init_ships_to_add [] in
  let player =
    {
      name;
      ship_board;
      attacked_board = init_attacked_board;
      ship_health = init_ship_health;
    }
  in
  player

let initialize_game_state () =
  print_endline "Welcome to Battleship!";
  print_string "Player 1, enter your name: ";
  let player1_name = read_line () in
  print_string "Player 2, enter your name: ";
  let player2_name = read_line () in
  print_endline "";
  let personalized_welcome =
    "Welcome, " ^ player1_name ^ " and " ^ player2_name ^ ". "
  in
  print_endline
    (personalized_welcome ^ "Let’s add some ships, " ^ player1_name
   ^ " goes first!");
  print_endline "";
  let player1 = initialize_player player1_name in
  print_endline (player1_name ^ ", here is your final board:");
  print_ship_board player1.ship_board;
  print_endline (player2_name ^ ", let's add your ships now!");
  let player2 = initialize_player player2_name in
  print_endline (player2_name ^ ", here is your final board:");
  print_ship_board player2.ship_board;
  let player1 = { player1 with attacked_board = player2.ship_board } in
  let player2 = { player2 with attacked_board = player1.ship_board } in
  let state = { player1; player2 } in
  state

let _ =
  let game_state = initialize_game_state () in
  game_loop game_state
(*let player1_init_ship_board = print_endline ""; Battleship.init_board in let
  player1_attacked_board = Battleship.init_board in let battleship1 = {
  ship_type = Battleship; orientation = Down; start_location = (4, 8) } in let
  carrier1 = { ship_type = Carrier; orientation = Right; start_location = (2, 1)
  } in let cruiser1 = { ship_type = Cruiser; orientation = Up; start_location =
  (9, 2) } in let submarine1 = { ship_type = Submarine; orientation = Left;
  start_location = (4, 5) } in let destroyer1 = { ship_type = Destroyer;
  orientation = Left; start_location = (0, 9) } in let player1_ship_board =
  add_ship player1_init_ship_board battleship1 in let player1_ship_board =
  add_ship player1_ship_board carrier1 in let player1_ship_board = add_ship
  player1_ship_board cruiser1 in let player1_ship_board = add_ship
  player1_ship_board submarine1 in let player1_ship_board = add_ship
  player1_ship_board destroyer1 in let player1 = { name = "Kevin"; ship_board =
  player1_ship_board; attacked_board = player1_attacked_board; ship_health =
  init_ship_health; } in let player2_init_ship_board = Battleship.init_board in

  let player2_attacked_board = Battleship.init_board in let battleship2 = {
  ship_type = Battleship; orientation = Left; start_location = (9, 9) } in let
  carrier2 = { ship_type = Carrier; orientation = Right; start_location = (0, 0)
  } in let cruiser2 = { ship_type = Cruiser; orientation = Up; start_location =
  (4, 5) } in let submarine2 = { ship_type = Submarine; orientation = Down;
  start_location = (2, 6) } in let destroyer2 = { ship_type = Destroyer;
  orientation = Left; start_location = (4, 9) } in let player2_ship_board =
  add_ship player2_init_ship_board battleship2 in let player2_ship_board =
  add_ship player2_ship_board carrier2 in let player2_ship_board = add_ship
  player2_ship_board cruiser2 in let player2_ship_board = add_ship
  player2_ship_board submarine2 in let player2_ship_board = add_ship
  player2_ship_board destroyer2 in let player2 = { name = "Michael"; ship_board
  = player2_ship_board; attacked_board = player2_attacked_board; ship_health =
  init_ship_health; } in let player1 = { player1 with attacked_board =
  player2.ship_board } in let player2 = { player2 with attacked_board =
  player1.ship_board } in game_loop { player1; player2 } *)
