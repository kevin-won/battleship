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
  occupation: occupation; 
  attacked: attacked;
}

type ship_board = block list list

type attacked_board = attacked list list

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
let print_ship_board ship_board = 
  let helper_board row = 
    List.iter (fun elem -> 
      match elem.occupation with 
      | Unoccupied -> 
        (match elem.attacked with 
          | Hit -> raise ThisWillNeverHappen
          | Miss -> print_string " O "
          | Sunk -> raise ThisWillNeverHappen
          | Not -> print_string " * ")
      | Occupied ship_type -> 
        match elem.attacked with 
          | Hit -> print_string " X "
          | Miss -> raise ThisWillNeverHappen
          | Sunk -> print_string " S "
          | Not -> print_string (ship_logo ship_type)
      ) row;
    print_newline () in 
  List.iter helper_board ship_board

(** [elements_at_col] returns the elements at column [col] in [ship_board] in reversed order. *)
let rec elements_at_col ship_board col acc = 
  match ship_board with 
  | [] -> acc
  | row :: rest_of_board -> elements_at_col rest_of_board col (List.nth row col :: acc)

(** [ship_overlaps_helper] returns true if at least one element in [row] 
    between [i1] and [i2], inclusive, is [Occupied].  *)
let ship_overlaps_helper i1 i2 row = 
  let true_false_lst = List.mapi (fun i elem -> if i1 <= i && i <= i2 then 
    match elem.occupation with 
      | Unoccupied -> false
      | Occupied _ -> true 
  else false) row in 
  List.exists (fun x -> x = true) true_false_lst

let ship_overlaps ship_board size row col orientation = 
  match orientation with 
  | Left -> 
      let specific_row = List.nth ship_board row in 
      ship_overlaps_helper (col-size+1) col specific_row
  | Right -> 
      let specific_row = List.nth ship_board row in 
      ship_overlaps_helper col (col+size-1) specific_row
  | Up ->
      let col_lst = List.rev (elements_at_col ship_board col []) in
      ship_overlaps_helper (row-size+1) row col_lst
  | Down -> 
      let col_lst = List.rev (elements_at_col ship_board col []) in
      ship_overlaps_helper row (row+size-1) col_lst

let add_ship ship_board ship = 
  let row = fst ship.start_location in
  let col = snd ship.start_location in 
  let ship_type = ship.ship_type in
  let orientation = ship.orientation in
  let size = ship_size ship_type in 
    match orientation with 
    | Left -> 
        List.mapi (fun i r -> 
          if i <> row then r else
          List.mapi (fun k elem -> 
            if k < col-size+1 || k > col then elem else
              {occupation=Occupied ship_type; attacked=Not}
            ) r 
          ) ship_board 
    | Right ->
        List.mapi (fun i r -> 
          if i <> row then r else
          List.mapi (fun k elem -> 
            if k < col || k > col+size-1 then elem else
              {occupation=Occupied ship_type; attacked=Not}
            ) r 
          ) ship_board 
    | Up ->
        List.mapi (fun i r -> 
          if i < row-size+1 || i > row then r else
          List.mapi (fun k elem -> 
            if k = col then {occupation=Occupied ship_type; attacked=Not} else
              elem
            ) r
          ) ship_board
    | Down -> 
        List.mapi (fun i r -> 
          if i < row || i > row+size-1 then r else
          List.mapi (fun k elem -> 
            if k = col then {occupation=Occupied ship_type; attacked=Not} else
              elem
            ) r
          ) ship_board

let remove_ship ship_board ship_type =
  List.map (fun row -> 
    List.map (fun elem -> 
      match elem.occupation with 
      | Occupied ship -> if ship <> ship_type then elem else {occupation=Unoccupied; attacked=Not}
      | Unoccupied -> elem
      ) row
    ) ship_board

(* ---------------------------------------Start-Here--------------------------------------------- *)

(* let attack board row col : ship_board = 
  List.mapi (fun i r -> 
    if i <> row then r else
      List.mapi (fun k elem -> 
        if k <> col then elem else
          match elem.occupation with 
          | Occupied _ -> {elem with attacked=Hit} 
          | Unoccupied -> {elem with attacked=Miss} 
        ) r
    ) board *)

