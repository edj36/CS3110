module Turn : sig

  type state
  type player

  val get_current : state -> player

  val get_next : state -> player

  (*val get_move : int -> string *)

  val next_turn : state -> state

  val repl : move -> unit

end
