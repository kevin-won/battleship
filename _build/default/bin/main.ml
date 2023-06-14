open Battleship 

type person = {
  my_board : board;
  opponent_board : board;
} 

let initialize_player_board () =
  print_endline "It's time to add ships for player 1!";
  let person_1 = {my_board = Battleship.my_board; opponent_board = Battleship.opponent_board} in 
  print_endline "Please insert commands as follows: Add <Ship> facing <orientation> at <(x,y)>";
  print_string "Add the carrier: ";
  let carrier = read_line () in 
  print_string "Add the battleship: ";
  let battleship = read_line () in 
  print_string "Add the cruiser: ";
  let cruiser = read_line () in 
  print_string "Add the submarine: ";
  let submarine = read_line () in 
  print_string "Add the destroyer: ";
  let destroyer = read_line () in  
  let lst = [carrier; battleship; cruiser; submarine; destroyer] in

let initialize_game_state () = 
  print_endline "Welcome to Battleship!";
  print_string "What's player 1's name? ";
  let player_1_name = read_line () in 
  print_string "What's player 2's name? ";
  let player_2_name = read_line() in 



  let player_one_board = initialize_player_board () in
  let player_two_board = initialize_player_board () in 
  



let render_board game_state =

let process_move game_state move =

let rec game_loop game_state = 
  render_board game_state;

  let move = read_line () in 
  let new_game_state = process_move game_state move in 

  if game_over new_game_state then 
    ()
  else 
    game_loop (new_game_state)

let main () =
  let game_state = initialize_game_state () in
  game_loop game_state

let _ = main ()