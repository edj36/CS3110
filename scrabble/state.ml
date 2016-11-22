open Data

module State = struct

  (* type for game state *)
  type state = game_state

  (* type for a move in the game *)
  type move = moves

  (* type to store all players of the game *)
  type players = player list

  (* [setup] is the initial [state] of the game constructed from [player t] *)
  let setup players = failwith "Unimplemented"

  (* [update] is the new [state] resulting from evaluation of [move] in the
   * current [state] *)
  let update move state = failwith "Unimplemented"

end
