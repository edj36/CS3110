(* [AI] is an abstraction for the computer simulated player in the 
 * scrabble game. [AI] evaluates possible moves and submits to the game 
 * the same as a human would when playing. *)
module AI : sig

  type t

  (* type for game state *)
  type state

  (* type for a move in the game *)
  type move

  (* [search_move] is all possible moves that [AI] can make in [state] *)
  val search_move : state -> move t

  (* [draw_letter] is the letter [AI] gets when it cannot find a 
   * possible move in [state] *)
  val draw_letter : state -> move

  (* [get_score] is the score of [move] *)
  val get_score : move -> state -> int

  (* [submit_move] enters [move] to the game and is the [state] resulting from 
   * [move]'s' execution *)
  val submit_move : state -> move -> state

end
