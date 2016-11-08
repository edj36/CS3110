module Turn : sig

  type game_state
  type player

  val get_next : game_state -> player

  val get_move : int -> string -> 
end
