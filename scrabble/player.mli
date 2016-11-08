(*
 * signature for player
 * control but limit the operations that clients can make
 *)

module Player : sig

  type state
  type move

  val make_move : int -> int -> string -> move

  val draw_letter : state -> state

  val get_scote : state -> int

  val submit_move : state -> move -> state


end
