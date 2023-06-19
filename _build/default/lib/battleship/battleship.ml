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
| Not

type cell = {
  occupation: occupation; 
  attacked: attacked;
}

type board = cell list list

(* ---------------------------------------Game-Exceptions--------------------------------------------- *)

exception OutOfBounds
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
let print_board board = 
  let helper_board row = 
    List.iter (fun elem -> 
      match elem.occupation with 
      | Unoccupied -> 
        (match elem.attacked with 
          | Hit -> raise ThisWillNeverHappen
          | Miss -> print_string " O "
          | Not -> print_string " * ")
      | Occupied ship_type -> 
        match elem.attacked with 
          | Hit -> print_string " X "
          | Miss -> raise ThisWillNeverHappen
          | Not -> print_string (ship_logo ship_type)
      ) row;
    print_newline () in 
  List.iter helper_board board

let in_bounds row col ship_size orientation = 
  match orientation with 
  | Left -> if col + 1 - ship_size < 0 then false else true
  | Right ->if col - 1 + ship_size > 9 then false else true
  | Up -> if row + 1 - ship_size < 0 then false else true
  | Down -> if row - 1 + ship_size > 9 then false else true

let overlaps_in_bounds i1 i2 row = 
  let t_f_lst = List.mapi (fun i elem -> if i1 <= i && i <= i2 then 
    match elem.occupation with 
      | Unoccupied -> false
      | Occupied _ -> true 
  else false) row in 
  List.exists (fun x -> x = true) t_f_lst

let rec extract_rev_col_lst board col acc = 
  match board with 
  | [] -> acc
  | h :: t -> extract_rev_col_lst t col (List.nth h col :: acc)

let ship_overlaps board size row col orientation = 
  match orientation with 
  | Left -> 
      let spec_row = List.nth board row in 
      overlaps_in_bounds (col-size+1) col spec_row
  | Right -> 
      let spec_row = List.nth board row in 
      overlaps_in_bounds col (col+size-1) spec_row
  | Up ->
      let col_lst = List.rev (extract_rev_col_lst board col []) in
      overlaps_in_bounds (row-size+1) row col_lst
  | Down -> 
      let col_lst = List.rev (extract_rev_col_lst board col []) in
      overlaps_in_bounds row (row+size-1) col_lst

let game_dimension = 10

let add_ship board ship : board = 
  let row = fst ship.start_location in
  let col = snd ship.start_location in 
  let ship_type = ship.ship_type in
  let orientation = ship.orientation in
  if row < 0 || row > 9 || col < 0 || col > 9 then raise OutOfBounds else
    let size = ship_size ship_type in 
    if not (in_bounds row col size orientation) then raise OutOfBounds else 
      if (ship_overlaps board size row col orientation) then raise ShipOverlaps else 
        match orientation with 
        | Left -> 
            List.mapi (fun i r -> 
              if i <> row then r else
              List.mapi (fun k elem -> 
                if k < col-size+1 || k > col then elem else
                  {occupation=Occupied ship_type; attacked=Not}
                ) r 
              ) board 
        | Right ->
            List.mapi (fun i r -> 
              if i <> row then r else
              List.mapi (fun k elem -> 
                if k < col || k > col+size-1 then elem else
                  {occupation=Occupied ship_type; attacked=Not}
                ) r 
              ) board 
        | Up ->
            List.mapi (fun i r -> 
              if i < row-size+1 || i > row then r else
              List.mapi (fun k elem -> 
                if k = col then {occupation=Occupied ship_type; attacked=Not} else
                  elem
                ) r
              ) board
        | Down -> 
            List.mapi (fun i r -> 
              if i < row || i > row+size-1 then r else
              List.mapi (fun k elem -> 
                if k = col then {occupation=Occupied ship_type; attacked=Not} else
                  elem
                ) r
              ) board

let remove_ship board ship_type =
  List.map (fun row -> 
    List.map (fun elem -> 
      match elem.occupation with 
      | Occupied ship -> if ship <> ship_type then elem else {occupation=Unoccupied; attacked=Not}
      | Unoccupied -> elem
      ) row
    ) board

let attack board row col : board = 
  List.mapi (fun i r -> 
    if i <> row then r else
      List.mapi (fun k elem -> 
        if k <> col then elem else
          match elem.occupation with 
          | Occupied _ -> {elem with attacked=Hit} 
          | Unoccupied -> {elem with attacked=Miss} 
        ) r
    ) board

