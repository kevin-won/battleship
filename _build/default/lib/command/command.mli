(** [add_or_remove_ship_error] represents the four kind of errors that can occur
    when a user tries to add or remove a ship: the command is empty; the command is
    not in the right format; the ship to be removed is not on the board; the ship
    to be added is already on the board. *)
type add_or_remove_ship_error =
| Empty
| Malformed
| CannotAdd
| CannotRemove

(** [add_or_remove_ship_outcome] represents the three outcomes that can occur when
    a user tries to add or remove a ship: the user successfuly adds or removes a ship,
    or cannot add nor remove a ship due to some error. Such an error would be of type
    [add_or_remove_ship_error]. *)
type add_or_remove_ship_outcome = 
| Add of ship_type
| Remove of ship_type
| Error of add_or_remove_ship_error

(** [valid_add_or_remove_ship_command] returns 
        a) [Add ship_type] if [command] is a valid command to add a ship
            to the ship board and [ship] is in [ships_to_add]. A valid command 
            is defined as a case-insensitive string that contains two words, where
            a word is defined as a consecutive sequence of non-space characters. 
            Those two words are "add" and "<ship_type>", in that order. The string 
            may contain leading, internal, or trailing spaces.
        b) [Error CannotAdd] if [command] is a valid command to add a ship to the
            ship board, but [ship] is not in [ships_to_add].
        c) [Remove ship_type] if the same rules as (a) are followed, replacing
            "add" with "remove".
        d) [Error CannotRemove] if [command] is a valid command to remove a ship from 
            the ship board, but [ship] is not in [ships_added].
        e) [Error Empty] if [command] is an empty string.
        f) [Error Malformed] in all other cases. *)
val valid_add_or_remove_ship_command : string -> ship_type list -> ship_type list -> add_or_remove_ship_command_one

(** [orientation_and_location_error] represents the six kind of errors that can occur
    when a user tries to add or remove a ship: the command is empty; the command is
    not in the right format; the start location is not a number; the start location is
    a number, but it is outside the bounds of the board; the ship added will spill off 
    the board; the new ship overlaps with a previously added ship. *)
type orientation_and_location_error =
| Empty
| Malformed
| NotANumber
| OutOfBounds
| ShipDoesNotFit
| ShipOverlaps

(** [orientation_and_location_outcome] represents the two outcomes that can occur when
    a user tries to input an orientation and starting location to add a ship: the
    user successfully inputs the information, or cannot due to some error. Such an 
    error would be of type [orientation_and_location_error]. *)
type orientation_and_location_outcome = 
| Valid of orientation * int * int
| Error of orientation_and_location_error

(** [valid_orientation_and_location_command] returns 
        a) [Valid (<orientation>, row, col)] if [command] is a valid command for the 
            orientation and start location of a ship when added to ship board. A valid 
            command is defined as a case-insensitive string that contains three words, 
            where a word is defined as a consecutive sequence of non-space characters. 
            Those three words are "<orientation>", "at", "<start-location>", in that order.
            <orientation> must be one of "left", "right", "up", "down" and <start-location>
            must be "(row,col)" where row and col are integers from 0 to 9, inclusive. The 
            string may contain leading, internal, or trailing spaces between the words.
        b) [Error Empty] if [command] is an empty string.
        c) [Error NotANumber] if the <start-location> is not written as two ints.
        d) [Error OutOfBounds] if the <start-location> is not on the board.
        e) [Error ShipDoesNotFit] if the ship at <start-location> facing <orientation>
            spills outside the board.
        f) [Error ShipOverlaps] if the ship at <start-location> facing <orientation> 
            collides with another ship on the board.
        g) [Error Malformed] in all other cases. *)
val valid_orientation_and_location_command : string -> ship_type -> ship_board -> orientation_and_location_outcome