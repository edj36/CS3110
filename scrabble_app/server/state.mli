(* A [State] is the current game board and all associated data of the
 * scrabble game. *)

(* [initilize_board] is a tile array array representation of game board *)
val initilize_board : unit -> Data_t.tile list list 

(* [setup] is the initial [state] of the game constructed from [player list] *)
val setup : string -> string

(* [update] is the new [state] resulting from evaluation of [move] in the
 * current [state] *)
val update : Data_t.move -> Data_t.game_state -> Data_t.game_state
