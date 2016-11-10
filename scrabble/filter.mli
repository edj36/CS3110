(* A [Filter] is an abstraction that determines validity of any given
 * move within any given state. *)
module Filter : sig

  (* type for the game state *)
  type state

  (* type for a move in the game *)
  type move

  (* type for the dictionary of possible scrabble words *)
  type dictionary

  (* [validate] is the boolean value indicating the validity (true if valid) 
   * of [move] within [state], given scrabble dictionary [dictionary] *)
  val validate : state -> move -> dictionary -> bool

  (* [send] is the new [state] resulting evaluating [move] in [state] *)
  val send : state -> move -> state

end
