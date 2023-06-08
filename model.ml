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
| Occupied of ship
| Unoccupied

type attacked = 
| Hit
| Miss
| Not

type cell = {
  occupation: occupation; 
  attacked: attacked
}

let game_dimension = 10

type board = cell list

let board = List.init (game_dimension*game_dimension) (fun x -> {occupation=Unoccupied; attacked=Not})

exception ThisWillNeverHappen

let rec string_board board = 
  match board with 
  | [] -> ""
  | a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: t -> " * * * * * * * * * * \n" ^ string_board t
  | _ -> raise ThisWillNeverHappen

let _ = print_string (string_board board)

let ship_size ship_type =
  match ship_type with 
  | Carrier -> 5
  | Battleship -> 4
  | Cruiser -> 3
  | Submarine -> 3
  | Destroyer -> 2

exception OutOfBounds

let in_bounds row col ship_size orientation = 
  match orientation with 
  | Left -> if col + 1 - ship_size < 0 then false else true
  | Right ->if col - 1 + ship_size > 9 then false else true
  | Up -> if row + 1 - ship_size < 0 then false else true
  | Down -> if row - 1 + ship_size > 9 then false else true






