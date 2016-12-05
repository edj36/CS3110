(* A [GUI] is the client facing interface for the scrabble game. Upon each
 * move, [GUI] needs to reflect changes in player's score, who's turn it
 * is, changes in the player's tiles, and changes on the game board. *)

 (* [update_gui] Data.game_state -> unit
  * prints current state on terminal. take current game state and check following
  * information.
  * 1, Current turn (adds 1 before printing because turn starts from 0 )
  * 2, Current player's name
  * 3, Total number of letters left in the bag
  * 4, Print current game board
  * 5, Print command rule
  * 6, print score board
  * 7, player's hand *)
val update_gui: Data.game_state -> unit
