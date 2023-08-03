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

type board = block list list
(** [board] represents the ship board the player will add his ships to and track
    his opponent's moves on, and it will also represent the attacked board where
    the player will track his moves on against the opponent. *)

type ship_health = {
  carrier : int;
  battleship : int;
  cruiser : int;
  submarine : int;
  destroyer : int;
}
(** [ship_health] represents the health of each ship, measured in terms of hits
    left before being sunk. *)

(* ---------------------------------------Game-Exceptions--------------------------------------------- *)

(** All exceptions defined below are specified in [command.mli]. *)

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

val ship_size : ship_type -> int
(** [ship_size] returns the size of a ship. *)

val ship_logo : ship_type -> string
(** [ship_logo] returns the logo of a ship to be displayed on the ship board. *)

val ship_name : ship_type -> string
(** [ship_name] returns the name of a ship. *)

(* ---------------------------------------Game-Functions--------------------------------------------- *)

val init_board : board
(** [init_board] creates an empty board. *)

val init_ship_health : ship_health
(** [init_ship_health] defines a record that sets the health of each ship to its
    maximum. *)

val init_ships_to_add : ship_type list
(** [init_ships_to_add] represents the ships ready to be added to ship board at
    the start of the game. *)

val print_ship_board : board -> unit
(** [print_ship_board] prints the ship board based off block occupation and
    attacked state. *)

val print_attacked_board : board -> unit
(** [print_attacked_board] prints the attacked board based off attacked state. *)

val add_ship : board -> ship -> board
(** [add_ship] returns the new ship board after adding [ship] to [ship_board]. *)

val remove_ship : board -> ship_type -> board
(** [remove_ship] returns the new ship board after removing [ship] from
    [ship_board]. *)

val attack :
  board ->
  board ->
  ship_health ->
  position ->
  board * board * ship_health * attacked
(** [attack] returns a 4-tuple, where the first entry is the new attacked board
    for the player attacking, the second entry is the new ship board for the
    opponent, the third entry is the new ship health for the opponent, and the
    fourth entry indicates the attack outcome. *)

val game_over : ship_health -> bool
(** [game_over] returns [true] if all the fields in [ship_health] are set to 0. *)

(* ---------------------------------------HELPERS--------------------------------------------- *)

val ship_overlaps : board -> ship -> bool
(** [ship_overlaps] returns [true] if [ship] collides with a previously added
    ship on [ship_board]; returns [false] otherwise. Precondition: the ship
    added at [ship.start_location] facing [ship.orientation] must fit on board. *)

val ship_fits : ship -> bool
(** [ship_fits] returns [true] if [ship] fits on board, given that its starting
    location is at [ship.start_location] and it faces [ship.orientation];
    [false] otherwise. Precondition: [start_location] is a valid start location. *)

val get_value_at : board -> position -> block
(** [get_value_at] returns the block at [coordinates] in the 2D list, [board]. *)
