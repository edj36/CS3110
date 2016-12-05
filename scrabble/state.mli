(* A [State] is the current game board and all associated data of the
 * scrabble game. *)

(* [setup] is the initial [state] of the game constructed from [player list] *)
val setup : Data.player list -> Data.game_state

(* [update] is the new [state] resulting from evaluation of [move] in the
 * current [state] *)
val update : Data.move -> Data.game_state -> Data.game_state
