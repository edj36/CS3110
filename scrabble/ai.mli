(* AI signature *)
module AI : sig

  (* date type for game state *)
  type state
  (* data type for the move AI makes *)
  type move

  (* search_move will search any possible moves that AI can make *)
  val search_move : state -> move t

  (* draw_letter will make AI draw one letter from letter_bag *)
  val draw_letter : state -> move

  (* get_scote will evaluate the score of the move *)
  val get_scote : move -> int

  (* evaluate_move will select the AI's move amoung possible moves *)
  val evaluate: move t -> move

  (* submit_move  will update the state after selecting AI's move *)
  val submit_move : state -> move -> state

  (* turn will update the state when it is AI's turn. take current state as an
   * input, and update the state according to AI's move. AI turn will be force quit
   * after certain words are searched. Number of words to search is the second
   * input of this function *)
  val turn : state -> int -> state

end
