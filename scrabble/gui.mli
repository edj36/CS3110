(* A [GUI] is the client facing interface for the scrabble game. Upon each
 * move, [GUI] needs to reflect changes in player's score, who's turn it
 * is, changes in the player's tiles, and changes on the game board. *)

(* [main_manu] : unit -> unit
 * [main_manu] displays a main manu of the game. Waits for the string input
 * and moves on to next window based on the input
 * [play] -> call initialize_game
 * [help] -> displays tutorial and waits for the next string
 * [quit] -> quit the game *)
val main_menu : unit -> unit
