(* A [Server] is the backend of the scrabble game. 
module Server : sig

  (* type for the game state *)
  type state

  (* type for a move in the game *)
  type move

  (* type for the gui in the game *)
  type gui

  (* [update] is the new [state] resulting from evaluating [move] in the 
   * current [state] *)
  val update : move -> state -> state

  (* [alert_gui] is a unit resulting from printing predetermined contents of 
   * [state] via [gui] for the client to view *)
  val alert_gui : state -> gui -> unit
  
end*)
