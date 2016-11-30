(* A [GUI] is the client facing interface for the scrabble game. Upon each
 * move, [GUI] needs to reflect changes in player's score, who's turn it
 * is, changes in the player's tiles, and changes on the game board. *)


(* [update] is the unit value resulting from updating the interface
 * to display the new [state]
 * Requires:
 * - displays current player's score
 * - displays who's turn it is
 * - displays current player's tiles
 * - displays current game board *)

val main_menu : unit -> unit
