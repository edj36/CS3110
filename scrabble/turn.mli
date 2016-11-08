module Turn : sig

  type state
  type player

  val get_current : state -> player

  val get_next : state -> player

  val get_move : int -> string ->

  val next : state -> state
  
end
