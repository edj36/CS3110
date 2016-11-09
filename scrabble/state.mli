(* Signature for game state *)
module State : sig

  type game_state

  val update : turn -> game_state -> game_state

end
