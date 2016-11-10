(* AI signature *)
module AI : sig

  (* date type for game state *)
  type state
  (* data type for the move AI makes *)
  type move

  (* [search_move] will search any possible moves that AI can make *)
  val search_move : state -> move t

  (* [draw_letter] will make AI draw one letter from letter_bag *)
  val draw_letter : state -> move

  (* [get_scote] will evaluate the score of the move *)
  val get_scote : move -> int

  (* [submit_move]  will update the state after selecting AI's move *)
  val submit_move : state -> move -> state

end
