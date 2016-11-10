(* Signature for Filter *)
module Filter : sig
  (* data type for the game state *)
  type state
  (* data type for the move *)
  type move
  (* data type for the dictionary*)
  type dictionary

  (* [validate] will evaluate the latest move by the player using dictionary module.
   * if returns true, the move is valid in the current state.
   * if it is false, then the move is invalid *)
  val validate : state -> move -> dictionary -> bool

  (* [send] will send the valid move to the server module and return updates state *)
  val send : state -> move -> state

end
