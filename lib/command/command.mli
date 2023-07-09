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
            to the ship board and [ship] is in [ships_to_add]. A valid [command] 
            is defined as a case-insensitive string that contains two words, where
            a word is defined as a consecutive sequence of non-space characters. 
            Those two words are "add" and "<ship_type>", and the order of the words 
            must be "add" followed by "<ship_type>". The string may contain leading, 
            internal, or trailing spaces.
        b) [Error CannotAdd] if [command] is a valid command to add a ship to the
            ship board, but [ship] is not in [ships_to_add].
        c) [Remove ship_type] if the same rules as (a) are followed, replacing
            "add" with "remove".
        d) [Error CannotRemove] if [command] is a valid command to remove a ship from 
            the ship board, but [ship] is not in [ships_added].
        e) [Error Empty] if [command] is an empty string.
        f) [Error Malformed] in all other cases. *)
val valid_add_or_remove_ship_command : string -> ship_type list -> ship_type list -> add_or_remove_ship_command_one

