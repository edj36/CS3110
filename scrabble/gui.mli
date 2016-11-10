(* Signature for GUI*)
module GUI : sig
  (* data type for the game state *)
  type state

  (* [update] will take the current game state and update GUI *)
  val update : state -> unit
  
end
