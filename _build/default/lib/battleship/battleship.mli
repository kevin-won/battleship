(** Representation of the game Battleship.

    This module defines the main components of Battleship, such as 
    the board and the ships, as well as key functions such as [add_ship]
    and [attack].
*)

(** [ship_type] represents the type of a ship. *)
type ship_type = 
| Carrier 
| Battleship
| Cruiser
| Submarine
| Destroyer  

(** [orientation] represents the orientation of a ship. *)
type orientation = 
| Left 
| Right
| Up
| Down

(** [position] represents the position of a block on a board. *)
type position = int * int

(** [ship] represents a ship. *)
type ship = {
  ship_type : ship_type;
  orientation : orientation;
  start_location : position;
}

(** [occupation] represents whether a block on the board is occupied with a ship or not. *)
type occupation = 
| Occupied of ship_type
| Unoccupied

(** [attacked] defines the four outcomes of a block on the board being attacked. *)
type attacked = 
| Hit
| Miss
| Sunk
| Not

(** [block] represents a block on the board. *)
type block = {
  occupation: occupation; 
  attacked: attacked;
}

(** [ship_board] represents the board the player will add his ships to and track his opponent's moves on. *)
type ship_board = block list list

(** [attacked_board] represents the board the player will track his own moves on against the opponent. *)
type attacked_board = attacked list list

(** Raised when a player adds a ship to his board that is out of bounds or attacks a block that's out of bounds. *)
exception OutOfBounds

(** Raised when a player adds a ship to his board that overlaps with a previously added ship. *)
exception ShipOverlaps

(** [ship_size] returns the size of a ship. *)
val ship_size : ship_type -> int
  
(** [ship_logo] returns the logo of a ship to be displayed on the ship board. *)
val ship_logo : ship_type -> string 
  
(** [ship_name] returns the name of a ship. *)
val ship_name : ship_type -> string

(** [print_ship_board] prints the ship board based off block occupation and attacked state. *)
val print_ship_board : ship_board -> unit

(** [ship_overlaps] returns [true] if new ship collides with a previously added ship and returns [false]
    otherwise. Precondition: the ship added at [row] and [col] with [orientation] must fit on board. *)
val ship_overlaps : ship_board -> int -> int -> int -> orientation -> bool

(** [add_ship] returns the new ship board after adding [ship] to [ship_board]. Precondition: [ship]
    must be able to be on [ship_board] following the rules of Battleship. *)
val add_ship : ship_board -> ship -> ship_board

(** [remove_ship] returns the new ship board after removing [ship] from [ship_board]. Precondition: [ship] 
    must be on [ship_board]. *)
val remove_ship : ship_board -> ship_type -> ship_board