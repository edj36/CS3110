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
  let update s m =
    match m with
    | Play {word = str; direction = dir; coordinate = crd}
      -> s
    | SwitchAll ->
      let player = current_player s in
      add_letter (snd player) s.letter_bag;
      let new_hand = (fst player, draw_letters 7 s.letter_bag) in
      let new_racks = update_racks new_hand s in
      {
        board = s.board;
        score_board = s.score_board;
        letter_bag = s.letter_bag;
        player_racks = new_racks;
        turn = s.turn + 1
      }
    | SwitchSome lst ->
      let letters = List.map (fun x -> char_to_letter x s.letter_bag) lst in
      let player = current_player s in
      add_letter letters s.letter_bag;
      let removed = List.filter (fun x -> not (List.mem x letters)) (snd player) in
      let new_hand =
        (fst player, removed @ (draw_letters (List.length letters) s.letter_bag)) in
      let new_racks = update_racks new_hand s in
      {
        board = s.board;
        score_board = s.score_board;
        letter_bag = s.letter_bag;
        player_racks = new_racks;
        turn = s.turn + 1
      }
    | Pass ->
      {
        board = s.board;
        score_board = s.score_board;
        letter_bag = s.letter_bag;
        player_racks = s.player_racks;
        turn = s.turn + 1
      }
    | Shuffle ->
      let player = current_player s in
      let indexed = List.map (fun x -> (Random.bits (), x)) (snd player) in
      let helper_sort x y = Pervasives.compare (fst x) (fst y) in
      let shuffled = List.map (fun x -> snd x) (List.sort helper_sort indexed) in
      let new_racks = update_racks ((fst player), shuffled) s in
      {
        board = s.board;
        score_board = s.score_board;
        letter_bag = s.letter_bag;
        player_racks = new_racks;
        turn = s.turn
      }
end
