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

type ship_board = block list list
type attacked_board = attacked list list

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
        match elem with
        | Hit -> print_orange " X "
        | Miss -> print_blue " O "
        | Sunk -> print_red " S "
        | Not -> print_string " * ")
      row;
    print_newline ()
  in
  List.iter helper_board attacked_board

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

let ship_overlaps ship_board size row col orientation =
  match orientation with
  | Left ->
      let specific_row = List.nth ship_board row in
      ship_overlaps_helper (col - size + 1) col specific_row
  | Right ->
      let specific_row = List.nth ship_board row in
      ship_overlaps_helper col (col + size - 1) specific_row
  | Up ->
      let col_lst = List.rev (elements_at_col ship_board col []) in
      ship_overlaps_helper (row - size + 1) row col_lst
  | Down ->
      let col_lst = List.rev (elements_at_col ship_board col []) in
      ship_overlaps_helper row (row + size - 1) col_lst

(** [in_bounds] returns [true] if starting at [(row,col)], facing [orientation],
    the ship will stay on the board according to [ship_size]. Precondition:
    [(row,col)] is a valid start location, i.e. 0 <= row, col <= 9. *)
let in_bounds row col ship_size orientation =
  match orientation with
  | Left -> if col + 1 - ship_size < 0 then false else true
  | Right -> if col - 1 + ship_size > 9 then false else true
  | Up -> if row + 1 - ship_size < 0 then false else true
  | Down -> if row - 1 + ship_size > 9 then false else true

let shipSatisfiesPrecondition ship_board ship =
  let row = fst ship.start_location in
  let col = snd ship.start_location in
  let ship_type = ship.ship_type in
  let orientation = ship.orientation in
  if row < 0 || row > 9 || col < 0 || col > 9 then raise OutOfBounds
  else
    let size = ship_size ship_type in
    if not (in_bounds row col size orientation) then raise ShipDoesNotFit
    else if ship_overlaps ship_board size row col orientation then
      raise ShipOverlaps
    else true

let add_ship ship_board ship =
  if shipSatisfiesPrecondition ship_board ship then
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
  else raise Empty1

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

(* ---------------------------------------Start-Here--------------------------------------------- *)

let get_value_at board row col =
  let row_list = List.nth board row in
  List.nth row_list col

let attack attacked_board opponent_ship_board coordinates =
  let row = fst coordinates in
  let col = snd coordinates in
  let opponent_ship_board =
    List.mapi
      (fun i r ->
        if i <> row then r
        else
          List.mapi
            (fun k elem ->
              if k <> col then elem
              else
                match elem.occupation with
                | Occupied _ -> { elem with attacked = Hit }
                | Unoccupied -> { elem with attacked = Miss })
            r)
      opponent_ship_board
  in
  let block = get_value_at opponent_ship_board row col in
  match block.occupation with
  | Unoccupied ->
      let attacked_board =
        List.mapi
          (fun i r ->
            if i <> row then r
            else List.mapi (fun k elem -> if k <> col then elem else Miss) r)
          attacked_board
      in
      (attacked_board, opponent_ship_board, false)
  | Occupied _ ->
      let attacked_board =
        List.mapi
          (fun i r ->
            if i <> row then r
            else List.mapi (fun k elem -> if k <> col then elem else Hit) r)
          attacked_board
      in
      (attacked_board, opponent_ship_board, true)
