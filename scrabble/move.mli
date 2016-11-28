
module type Move = sig

  type state

  type m

  val get_move : string -> m

  val submit_move: state -> m -> state

  val validate : string -> bool
  
end