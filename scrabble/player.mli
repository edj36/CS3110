(* A [Player] is a participant in the scrabble game. They can
 * submit moves to the board within the current state. *)
module Player : sig

  (* type for game state *)
  type t
  (* type for move *)
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
  val get_move : t -> move

end
