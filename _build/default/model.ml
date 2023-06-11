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
  mutable attacked: attacked;
}

type board = cell array array

let game_dimension = 10

let my_board : board = Array.init (game_dimension) (fun _ -> 
  Array.init (game_dimension) (fun _ -> {occupation=Unoccupied; attacked=Not}))

(* let opponent_board = Array.init (game_dimension) (fun _ -> 
  Array.init (game_dimension) (fun _ -> {occupation=Unoccupied; attacked=Not})) *)

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
  Array.iter (fun elem -> 
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
  Array.iter helper_board board

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

let ship_overlaps size row col orientation = 
  match orientation with 
  | Left -> 
      let overlaps = ref false in 
      for x=col-size+1 to col do
        if my_board.(row).(x).occupation <> Unoccupied then
          overlaps := true 
      done;
      !overlaps
  | Right ->
      let overlaps = ref false in 
      for x=col to col+size-1 do
        if my_board.(row).(x).occupation <> Unoccupied then
          overlaps := true 
      done;
      !overlaps
  | Up ->
      let overlaps = ref false in 
      for x=row-size+1 to row do 
        if my_board.(x).(col).occupation <> Unoccupied then
          overlaps := true 
      done;
      !overlaps
  | Down -> 
      let overlaps = ref false in 
      for x=row to row+size-1 do
        if my_board.(x).(col).occupation <> Unoccupied then
          overlaps := true 
      done;
      !overlaps

let add_ship ship = 
  let row = fst ship.start_location in
  let col = snd ship.start_location in 
  let ship_type = ship.ship_type in
  let orientation = ship.orientation in
  if row < 0 || row > 9 || col < 0 || col > 9 then raise OutOfBounds else
    let size = ship_size ship_type in 
    if not (in_bounds row col size orientation) then raise OutOfBounds else 
      if (ship_overlaps size row col orientation) then raise ShipOverlaps else 
        match orientation with 
        | Left -> 
          for x=col-size+1 to col do
            my_board.(row).(x) <- {occupation=Occupied ship_type; attacked=Not}
          done
        | Right ->
          for x=col to col+size-1 do
            my_board.(row).(x) <- {occupation=Occupied ship_type; attacked=Not}
          done
        | Up ->
            for x=row-size+1 to row do
              my_board.(x).(col) <- {occupation=Occupied ship_type; attacked=Not}
            done
        | Down -> 
            for x=row to row+size-1 do
              my_board.(x).(col) <- {occupation=Occupied ship_type; attacked=Not}
            done

let carrier = {ship_type=Carrier; orientation=Left; start_location=(1,9)}
let battleship = {ship_type=Battleship; orientation=Right; start_location=(3,5)}
let cruiser = {ship_type=Cruiser; orientation=Up; start_location=(8,9)}
let submarine = {ship_type=Submarine; orientation=Down; start_location=(3,1)}
let destroyer = {ship_type=Destroyer; orientation=Left; start_location=(9,3)}

let _ = add_ship carrier 

let _ = add_ship battleship 

let _ = add_ship cruiser 

let _ = add_ship submarine 

let _ = add_ship destroyer 

exception AlreadyAttacked

let attack row col = 
  if my_board.(row).(col).attacked = Not then 
    match my_board.(row).(col).occupation with 
    | Occupied _ -> my_board.(row).(col).attacked <- Hit
    | Unoccupied -> my_board.(row).(col).attacked <- Miss
  else raise AlreadyAttacked

let _ = attack 1 9

let _ = print_board my_board


