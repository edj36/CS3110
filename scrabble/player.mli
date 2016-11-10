(* signature for player
 * control but limit the operations that clients can make *)
module Player : sig
  (* date type for game state *)
  type state
  (* data type for the move player makes *)
  type move

  (* make_move will return type move based on user input
   * fist two [int] : the begining coordinate of word.
   * [char] : C or R (column or row, specifiying the direction of the word*)
  val make_move : int -> int -> char -> move

  (* draw_letter will be called when user decides to give up making a word and
   * draw a letter from the letterbag *)
  val draw_letter : state -> state

  (* get_score will be called after user types a word. it will return the
   * new score after submitting the currrent move *)
  val get_scote : state -> int

  (* submit_move will be called after confirming certain move by player.
   * the move will be sent to the server and update the game state *)
  val submit_move : state -> move -> state


end
