open Battleship

(* ----------------ADD-OR-REMOVE-SHIP "Add Carrier"------------------- *)

(** [add_or_remove_ship_command] represents the command to add a ship or remove
    a ship from the ship board. *)
type add_or_remove_ship_command =
  | Add of ship_type
  | Remove of ship_type

val valid_add_or_remove_ship_command :
  string -> ship_type list -> ship_type list -> add_or_remove_ship_command
(** [valid_add_or_remove_ship_command]

    (a) returns [Add ship_type] if [command] is a valid command to add a ship to
    the ship board and [ship_type] is in [ships_to_add]. A valid command is
    defined as a case-insensitive string that contains two words, where a word
    is defined as a consecutive sequence of non-space characters. Those two
    words are "add" and "<ship_type>", in that order. The string may contain
    leading, internal, or trailing spaces.

    b) raises the [CannotAdd] exception if [command] is a valid command to add a
    ship to the ship board, but [ship_type] is not in [ships_to_add].

    c) returns [Remove ship_type] if the same rules as (a) are followed,
    replacing "add" with "remove".

    d) raises the [CannotRemove] exception if [command] is a valid command to
    remove a ship from the ship board, but [ship_type] is not in [ships_added].

    e) raises the [Empty1] exception if [command] is an empty string.

    f) raises the [Malformed1] exception in all other cases. *)

(* --------------------LOCATION-AND-DIRECTION: "Left at
   (1,2)"------------------ *)

type orientation_and_location_command = orientation * position
(** [orientation_and_location_command] represents the command that details the
    orientation and starting position of a ship the player wants to add to the
    ship board. *)

val valid_orientation_and_location_command :
  string -> ship_type -> board -> ship
(** [valid_orientation_and_location_command] returns the new ship to be added to
    [ship_board] according to the details specified in [command] and
    [ship_type], if [command] is a valid command for the orientation and start
    location of a ship when added to [ship_board].

    A valid command is defined as a case-insensitive string that contains three
    words, where a word is defined as a consecutive sequence of non-space
    characters. Those three words are "<orientation>", "at", "<start-location>",
    in that order. <orientation> must be one of "left", "right", "up", "down"
    and <start-location> must be "(row,col)" where row and col are integers from
    0 to 9, inclusive. The string may contain leading, internal, or trailing
    spaces between the words.

    An exception will be raised if a command is not valid. Here are the kind of
    exceptions that can be raised:

    (a) [Empty2] if [command] is an empty string; (b) [NotANumber] if the
    command contains coordinates, but the coordinates are not written as
    integers; (c) [OutOfBounds] if the coordinates given are not from 0 to 9,
    inclusive; (d) [ShipDoesNotFit] if the ship at its start location facing its
    orientation spills outside the board; (e) [ShipOverlaps] if the ship at its
    start location facing orientation collides with another ship on
    [ship_board]; and (f) [Malformed2] is raised in all other cases *)

(** -------------------------ATTACK-------------------------*)

val valid_attack_command : string -> board -> position
(** [valid_attack_command] returns the coordinates expressed in
    [coordinates_command] if [coordinates_command] is valid, and raises an
    exception if invalid. Exact details are specified in
    [valid_orientation_and_location_command]. The only difference is that for
    the sake of error messages, instead of raising [Empty2] and [Malformed2], we
    raise [Empty3] and [Malformed3]. *)

(** -------------------------UTILITY-------------------------*)

val print_exception_message : exn -> unit
(** [print_exception_message] prints a message to the user depending on which
    exception [e] is thrown. *)
