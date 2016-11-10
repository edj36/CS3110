(* Signature for server *)
module Server : sig
  (* data type for the game state *)
  type state
  (* data type for the player *)
  type player
  (* data type for the gui *)
  type gui

  (* update will update the game state *)
  val update : state -> state

  (* alert_gui will notify the update to the gui module *)
  val alert_gui : state -> gui -> unit
  
end
