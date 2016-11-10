(* A [Player] is a participant in the scrabble game. They can 
 * submit moves to the board within the current state. *)
module Player : sig

  (* type for game state *)
  type state

  (* type for the move player makes *)
  type move

  (* [make_move] is the [move] based on user input and the [move] in 
   * in progress:
   * first [int] is x coordinate of [letter] 
   * second [int] is y coordinate of [letter]
   * [letter] is letter being put on the board at the above coordinates 
   * [make_move] adds the coordinate-letter combination to the list of 
   * existing coordinate-letter combinations already inside of the 
   * argument [move] 
   * Requires: 
   * [move] is of type Move within the move variant *)
  val make_move : int -> int -> letter -> move -> move 
  
  (* [submit_move] enters [move] to the game and is the [state] resulting from 
   * [move]'s' execution *)
  val submit_move : state -> move -> state

end
