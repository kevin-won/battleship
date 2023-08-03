(* ---------------------------------------Game-Types--------------------------------------------- *)
type ship_type =
  | Carrier
  | Battleship
  | Cruiser
  | Submarine
  | Destroyer

type orientation =
  | Left
  | Right
  | Up
  | Down

type position = int * int

type ship = {
  ship_type : ship_type;
  orientation : orientation;
  start_location : position;
}

type occupation =
  | Occupied of ship_type
  | Unoccupied

type attacked =
  | Hit
  | Miss
  | Sunk
  | Not

type block = {
  occupation : occupation;
  attacked : attacked;
}

type board = block list list

type ship_health = {
  carrier : int;
  battleship : int;
  cruiser : int;
  submarine : int;
  destroyer : int;
}

(* ---------------------------------------Game-Exceptions--------------------------------------------- *)

exception Empty1
exception Empty2
exception Empty3
exception Malformed1
exception Malformed2
exception Malformed3
exception CannotAdd
exception CannotRemove
exception NotANumber
exception OutOfBounds
exception ShipDoesNotFit
exception ShipOverlaps
exception AlreadyAttacked
exception ThisWillNeverHappen

(* ---------------------------------------Utilities--------------------------------------------- *)

let ship_size ship_type =
  match ship_type with
  | Carrier -> 5
  | Battleship -> 4
  | Cruiser -> 3
  | Submarine -> 3
  | Destroyer -> 2

let ship_logo ship_type =
  match ship_type with
  | Carrier -> " A "
  | Battleship -> " B "
  | Cruiser -> " C "
  | Submarine -> " D "
  | Destroyer -> " E "

let ship_name ship_type =
  match ship_type with
  | Carrier -> "Carrier"
  | Battleship -> "Battleship"
  | Cruiser -> "Cruiser"
  | Submarine -> "Submarine"
  | Destroyer -> "Destroyer"

(* ---------------------------------------Game-Functions--------------------------------------------- *)

let print_orange str = print_string ("\027[38;5;208m" ^ str ^ "\027[0m")
let print_red str = print_string ("\027[31m" ^ str ^ "\027[0m")
let print_blue str = print_string ("\027[34m" ^ str ^ "\027[0m")
let game_dimension = 10

let init_board =
  List.init game_dimension (fun _ ->
      List.init game_dimension (fun _ ->
          { occupation = Unoccupied; attacked = Not }))

let init_ship_health =
  { carrier = 5; battleship = 4; cruiser = 3; submarine = 3; destroyer = 2 }

let init_ships_to_add = [ Carrier; Battleship; Cruiser; Submarine; Destroyer ]

let print_ship_board ship_board =
  let helper_board row =
    List.iter
      (fun elem ->
        match elem.occupation with
        | Unoccupied -> (
            match elem.attacked with
            | Hit -> raise ThisWillNeverHappen
            | Miss -> print_blue " O "
            | Sunk -> raise ThisWillNeverHappen
            | Not -> print_string " * ")
        | Occupied ship_type -> (
            match elem.attacked with
            | Hit -> print_orange " X "
            | Miss -> raise ThisWillNeverHappen
            | Sunk -> print_red " S "
            | Not -> print_string (ship_logo ship_type)))
      row;
    print_newline ()
  in
  List.iter helper_board ship_board

let print_attacked_board attacked_board =
  let helper_board row =
    List.iter
      (fun elem ->
        match elem.occupation with
        | Unoccupied -> (
            match elem.attacked with
            | Hit -> raise ThisWillNeverHappen
            | Miss -> print_blue " O "
            | Sunk -> raise ThisWillNeverHappen
            | Not -> print_string " * ")
        | Occupied _ -> (
            match elem.attacked with
            | Hit -> print_orange " X "
            | Miss -> raise ThisWillNeverHappen
            | Sunk -> print_red " S "
            | Not -> print_string " * "))
      row;
    print_newline ()
  in
  List.iter helper_board attacked_board

let add_ship ship_board ship =
  let row = fst ship.start_location in
  let col = snd ship.start_location in
  let ship_type = ship.ship_type in
  let orientation = ship.orientation in
  let size = ship_size ship_type in
  match orientation with
  | Left ->
      List.mapi
        (fun i r ->
          if i <> row then r
          else
            List.mapi
              (fun k elem ->
                if k < col - size + 1 || k > col then elem
                else { occupation = Occupied ship_type; attacked = Not })
              r)
        ship_board
  | Right ->
      List.mapi
        (fun i r ->
          if i <> row then r
          else
            List.mapi
              (fun k elem ->
                if k < col || k > col + size - 1 then elem
                else { occupation = Occupied ship_type; attacked = Not })
              r)
        ship_board
  | Up ->
      List.mapi
        (fun i r ->
          if i < row - size + 1 || i > row then r
          else
            List.mapi
              (fun k elem ->
                if k = col then
                  { occupation = Occupied ship_type; attacked = Not }
                else elem)
              r)
        ship_board
  | Down ->
      List.mapi
        (fun i r ->
          if i < row || i > row + size - 1 then r
          else
            List.mapi
              (fun k elem ->
                if k = col then
                  { occupation = Occupied ship_type; attacked = Not }
                else elem)
              r)
        ship_board

let remove_ship ship_board ship_type =
  List.map
    (fun row ->
      List.map
        (fun elem ->
          match elem.occupation with
          | Occupied ship ->
              if ship <> ship_type then elem
              else { occupation = Unoccupied; attacked = Not }
          | Unoccupied -> elem)
        row)
    ship_board

let get_value_at board coordinates =
  let row = fst coordinates in
  let col = snd coordinates in
  let row_list = List.nth board row in
  List.nth row_list col

let lower_ship_health ship_health ship_type =
  match ship_type with
  | Carrier -> { ship_health with carrier = ship_health.carrier - 1 }
  | Battleship -> { ship_health with battleship = ship_health.battleship - 1 }
  | Cruiser -> { ship_health with cruiser = ship_health.cruiser - 1 }
  | Submarine -> { ship_health with submarine = ship_health.submarine - 1 }
  | Destroyer -> { ship_health with destroyer = ship_health.destroyer - 1 }

let ship_has_sunk ship_type ship_health =
  match ship_type with
  | Carrier -> ship_health.carrier = 0
  | Battleship -> ship_health.battleship = 0
  | Cruiser -> ship_health.cruiser = 0
  | Submarine -> ship_health.submarine = 0
  | Destroyer -> ship_health.destroyer = 0

let update_board_after_attack board coordinates attacked_outcome =
  let row = fst coordinates in
  let col = snd coordinates in
  List.mapi
    (fun i r ->
      if i <> row then r
      else
        List.mapi
          (fun k elem ->
            if k <> col then elem else { elem with attacked = attacked_outcome })
          r)
    board

let update_board_after_sunk board ship_type =
  List.map
    (fun row ->
      List.map
        (fun elem ->
          match elem.occupation with
          | Occupied ship ->
              if ship = ship_type then { elem with attacked = Sunk } else elem
          | Unoccupied -> elem)
        row)
    board

let game_over ship_health =
  ship_health.carrier = 0 && ship_health.battleship = 0
  && ship_health.cruiser = 0 && ship_health.destroyer = 0
  && ship_health.submarine = 0

let attack my_attacked_board opponent_ship_board opponent_ship_health
    coordinates =
  let block = get_value_at opponent_ship_board coordinates in
  match block.occupation with
  | Unoccupied ->
      let my_attacked_board =
        update_board_after_attack my_attacked_board coordinates Miss
      in
      let opponent_ship_board =
        update_board_after_attack opponent_ship_board coordinates Miss
      in
      (my_attacked_board, opponent_ship_board, opponent_ship_health, Miss)
  | Occupied ship_type ->
      let new_opponent_ship_health =
        lower_ship_health opponent_ship_health ship_type
      in
      let ship_just_sunk = ship_has_sunk ship_type new_opponent_ship_health in
      if not ship_just_sunk then
        let my_attacked_board =
          update_board_after_attack my_attacked_board coordinates Hit
        in
        let opponent_ship_board =
          update_board_after_attack opponent_ship_board coordinates Hit
        in
        (my_attacked_board, opponent_ship_board, new_opponent_ship_health, Hit)
      else
        let my_attacked_board =
          update_board_after_sunk my_attacked_board ship_type
        in
        let opponent_ship_board =
          update_board_after_sunk opponent_ship_board ship_type
        in
        (my_attacked_board, opponent_ship_board, new_opponent_ship_health, Sunk)

(*-----------------------------HELPERS--------------------------*)

(** [elements_at_col] returns the elements at column [col] in [ship_board] in
    reversed order. *)
let rec elements_at_col ship_board col acc =
  match ship_board with
  | [] -> acc
  | row :: rest_of_board ->
      elements_at_col rest_of_board col (List.nth row col :: acc)

(** [ship_overlaps_helper] returns true if at least one element in [row] between
    [i1] and [i2], inclusive, is [Occupied]. *)
let ship_overlaps_helper i1 i2 row =
  let true_false_lst =
    List.mapi
      (fun i elem ->
        if i1 <= i && i <= i2 then
          match elem.occupation with
          | Unoccupied -> false
          | Occupied _ -> true
        else false)
      row
  in
  List.exists (fun x -> x = true) true_false_lst

let ship_overlaps ship_board ship =
  let row = fst ship.start_location in
  let col = snd ship.start_location in
  let ship_size = ship_size ship.ship_type in
  match ship.orientation with
  | Left ->
      let specific_row = List.nth ship_board row in
      ship_overlaps_helper (col - ship_size + 1) col specific_row
  | Right ->
      let specific_row = List.nth ship_board row in
      ship_overlaps_helper col (col + ship_size - 1) specific_row
  | Up ->
      let col_lst = List.rev (elements_at_col ship_board col []) in
      ship_overlaps_helper (row - ship_size + 1) row col_lst
  | Down ->
      let col_lst = List.rev (elements_at_col ship_board col []) in
      ship_overlaps_helper row (row + ship_size - 1) col_lst

let ship_fits ship =
  let row = fst ship.start_location in
  let col = snd ship.start_location in
  let ship_size = ship_size ship.ship_type in
  match ship.orientation with
  | Left -> if col + 1 - ship_size < 0 then false else true
  | Right -> if col - 1 + ship_size > 9 then false else true
  | Up -> if row + 1 - ship_size < 0 then false else true
  | Down -> if row - 1 + ship_size > 9 then false else true
