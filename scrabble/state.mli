(* A [State] is the current game board and all associated data of the
 * scrabble game. *)
module State : sig

  (* type for game state *)
  type state

  (* type for a move in the game *)
  type move

  (* type to store all players of the game *)
  type players

  (* [setup] is the initial [state] of the game constructed from [player t] *)
  val setup : players -> state

  (* [update] is the new [state] resulting from evaluation of [move] in the
   * current [state] *)
  val update : move -> state -> state

end
