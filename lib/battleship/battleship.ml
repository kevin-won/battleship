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

let game_dimension = 10

let my_board : board = List.init (game_dimension) (fun _ -> 
  List.init (game_dimension) (fun _ -> {occupation=Unoccupied; attacked=Not}))

let opponent_board : board = List.init (game_dimension) (fun _ -> 
  List.init (game_dimension) (fun _ -> {occupation=Unoccupied; attacked=Not}))

exception ThisWillNeverHappen
exception OutOfBounds
exception ShipOverlaps

let ship_logo ship_type =
  match ship_type with 
  | Carrier -> " A "
  | Battleship -> " B "
  | Cruiser -> " C "
  | Submarine -> " D "
  | Destroyer -> " E "

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
  print_newline ()

let print_board board = 
  List.iter helper_board board

let ship_size ship_type =
  match ship_type with 
  | Carrier -> 5
  | Battleship -> 4
  | Cruiser -> 3
  | Submarine -> 3
  | Destroyer -> 2

let in_bounds row col ship_size orientation = 
  match orientation with 
  | Left -> if col + 1 - ship_size < 0 then false else true
  | Right ->if col - 1 + ship_size > 9 then false else true
  | Up -> if row + 1 - ship_size < 0 then false else true
  | Down -> if row - 1 + ship_size > 9 then false else true

let overlaps_helper i1 i2 row = 
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
      overlaps_helper (col-size+1) col spec_row
  | Right -> 
      let spec_row = List.nth board row in 
      overlaps_helper col (col+size-1) spec_row
  | Up ->
      let col_lst = List.rev (extract_rev_col_lst board col []) in
      overlaps_helper (row-size+1) row col_lst
  | Down -> 
      let col_lst = List.rev (extract_rev_col_lst board col []) in
      overlaps_helper row (row+size-1) col_lst

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

let attack board row col = 
  List.mapi (fun i r -> 
    if i <> row then r else
      List.mapi (fun k elem -> 
        if k <> col then elem else
          match elem.occupation with 
          | Occupied _ -> {elem with attacked=Hit} 
          | Unoccupied -> {elem with attacked=Miss} 
        ) r
    ) board

let carrier = {ship_type=Carrier; orientation=Left; start_location=(1,9)}
let battleship = {ship_type=Battleship; orientation=Right; start_location=(3,5)}
let cruiser = {ship_type=Cruiser; orientation=Up; start_location=(8,9)}
let submarine = {ship_type=Submarine; orientation=Down; start_location=(3,1)}
let destroyer = {ship_type=Destroyer; orientation=Left; start_location=(9,3)}

let a = add_ship my_board carrier 
let b = add_ship a battleship 
let c = add_ship b cruiser 
let d = add_ship c submarine 
let e = add_ship d destroyer 
let f = attack e 1 9
let _ = print_board f




