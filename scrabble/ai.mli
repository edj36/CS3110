(* AI signature *)
module AI : sig
  type state
  type hand

  val serach_move : state -> move

  val draw_letter : state -> state

  val get_scote : state -> int

  val submit_move : state -> move -> state

end
