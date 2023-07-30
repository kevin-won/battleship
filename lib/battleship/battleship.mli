(** Representation of the game Battleship.

    This module defines the main components of Battleship, such as the board and
    the ships, as well as key functions such as [add_ship] and [attack]. *)

(* ---------------------------------------Game-Types--------------------------------------------- *)

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

type position = int * int
(** [position] represents the position of a block on a board. *)

type ship = {
  ship_type : ship_type;
  orientation : orientation;
  start_location : position;
}
(** [ship] represents a ship. *)

(** [occupation] represents whether a block on the ship board is occupied with a
    ship or not. *)
type occupation =
  | Occupied of ship_type
  | Unoccupied

(** [attacked] defines the four outcomes of a block on the board being attacked. *)
type attacked =
  | Hit
  | Miss
  | Sunk
  | Not

type block = {
  occupation : occupation;
  attacked : attacked;
}
(** [block] represents a block on the ship board. *)

type ship_board = block list list
(** [ship_board] represents the board the player will add his ships to and track
    his opponent's moves on. *)

type attacked_board = attacked list list
(** [attacked_board] represents the board the player will track his own moves on
    against the opponent. *)

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

val ship_size : ship_type -> int
(** [ship_size] returns the size of a ship. *)

val ship_logo : ship_type -> string
(** [ship_logo] returns the logo of a ship to be displayed on the ship board. *)

val ship_name : ship_type -> string
(** [ship_name] returns the name of a ship. *)

(* ---------------------------------------Game-Functions--------------------------------------------- *)

val print_ship_board : ship_board -> unit
(** [print_ship_board] prints the ship board based off block occupation and
    attacked state. *)

val print_attacked_board : attacked_board -> unit
(** [print_attacked_board] prints the attacked board based off attacked state. *)

(* ---------------------------------------Start-Here--------------------------------------------- *)

val ship_overlaps : ship_board -> int -> int -> int -> orientation -> bool
(** [ship_overlaps] returns [true] if new ship collides with a previously added
    ship and returns [false] otherwise. Precondition: the ship added at [row]
    and [col] with [orientation] must fit on board. *)

val in_bounds : int -> int -> int -> orientation -> bool

(* ----------------------------BELOW THIS IS
   GOOD----------------------------------- *)

val add_ship : ship_board -> ship -> ship_board
(** [add_ship] returns the new ship board after adding [ship] to [ship_board]. *)

val remove_ship : ship_board -> ship_type -> ship_board
(** [remove_ship] returns the new ship board after removing [ship] from
    [ship_board]. *)

val attack :
  attacked_board -> ship_board -> position -> attacked_board * ship_board * bool
(** [attack] returns a 3-tuple, where the first entry is the new attacked board
    for the player attacking, the second entry is the new ship board for the
    player being attacked, and the third entry is a boolean value representing
    whether the attack resulted in a hit or not. *)
