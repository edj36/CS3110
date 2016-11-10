(* Signature for game state. Control and manage data structure we need for
 * the game play *)
module State : sig
  (* data type for the game state *)
  type state
  (* store all players of the game *)
  type player t

  (* [setup] will initialize the state. [player t] will represent the number
   * of players and AI(s) who play the game *)
  val setup : player t -> state

  (* [update] will update the state after one turn of game play. *)
  val update : state -> state

end
