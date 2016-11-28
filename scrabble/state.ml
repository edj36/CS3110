open Data
open API

module State = struct

  (* type for game state *)
  type state = game_state

  (* type for a move in the game *)
  type move = moves

  (* type to store all players of the game *)
  type players = player list


  (* [initial_bag] is a letter list representing the intial state of the game *)
  let letter_bag ()=
  [
    {character = 'A'; pt = 1; count = 9};
    {character = 'B'; pt = 3; count = 2};
    {character = 'C'; pt = 3; count = 2};
    {character = 'D'; pt = 2; count = 4};
    {character = 'E'; pt = 1; count = 12};
    {character = 'F'; pt = 4; count = 2};
    {character = 'G'; pt = 2; count = 3};
    {character = 'H'; pt = 4; count = 2};
    {character = 'I'; pt = 1; count = 9};
    {character = 'J'; pt = 8; count = 1};
    {character = 'K'; pt = 5; count = 1};
    {character = 'L'; pt = 1; count = 4};
    {character = 'M'; pt = 3; count = 2};
    {character = 'N'; pt = 1; count = 6};
    {character = 'O'; pt = 1; count = 8};
    {character = 'P'; pt = 3; count = 2};
    {character = 'Q'; pt = 10;count = 1};
    {character = 'R'; pt = 1; count = 6};
    {character = 'S'; pt = 1; count = 4};
    {character = 'T'; pt = 1; count = 6};
    {character = 'U'; pt = 1; count = 4};
    {character = 'V'; pt = 4; count = 2};
    {character = 'W'; pt = 4; count = 2};
    {character = 'X'; pt = 8; count = 1};
    {character = 'Y'; pt = 4; count = 2};
    {character = 'Z'; pt = 10;count = 1};
    {character = ' '; pt = 0; count = 2}
  ]

  (* [initialize_score] represents the tuple list of each player and their scores*)
  let rec initialize_score (players : player list) =
    match players with
    | [] -> []
    | h::t -> (h, 0):: initialize_score t

  let tw_coordinate =
  [(0,0);(0,7);(0,14);(7,0);(7,14);(14,0);(14,7);(14,14)]

  let tl_coordinate =
  [
  (1,5);(1,9);(5,1);(5,5);
  (5,9);(5,13);(9,1);(9,5);
  (9,9);(9,13);(13,5);(13,9)
  ]

  let dw_coordinate =
  [
  (1,1);(2,2);(3,3);(4,4);
  (10,4);(11,3);(12,2);(13,1);
  (4,10);(3,11);(2,12);(1,13);
  (10,10);(11,11);(12,12);(13,13)
  ]

  let dl_coordinate =
  [
  (0,3);(0,11);(2,6);(2,8);(3,7);
  (3,0);(11,0);(6,2);(8,2);(7,3);
  (14,3);(14,11);(12,6);(12,8);(11,7);
  (3,14);(11,14);(6,12);(8,12);(7,11);
  (6,6);(8,6);(8,8);(6,8)
  ]

  (* [initilize_board] is a tile array array representation of game board *)
  let initilize_board () =
    let board = Array.make_matrix 15 15 { bonus= Normal; letter = None } in
    board.(7).(7) <- { bonus= Center; letter = None };
    fill_coordinate tw_coordinate { bonus= Triple_word; letter = None } board;
    fill_coordinate tl_coordinate { bonus= Triple_letter; letter = None } board;
    fill_coordinate dw_coordinate { bonus= Double_word; letter = None } board;
    fill_coordinate dl_coordinate { bonus= Double_letter; letter = None } board;
    board

  (* [initialize_rack] is a (player * letter list) list, representating
   * each player's hands *)
  let rec initialize_rack (players: player list) bag =
    match players with
    | []-> []
    | h::t -> let hand = draw_letters 7 bag in (h, hand) :: initialize_rack t bag

  (* [setup] is the initial [state] of the game constructed from [player list] *)
  let setup (players : player list)=
    let initial_score = initialize_score players in
    let initial_board = initilize_board () in
    let initial_bag = letter_bag () in
    let racks = initialize_rack players initial_bag in
    {
      board = initial_board;
      score_board = initial_score;
      letter_bag = initial_bag;
      player_racks = racks;
      turn = 0
    }

  let update_racks hands s =
    let racks = s.player_racks in
    let rec helper hands racks =
    match racks with
    | [] -> []
    | h::t -> if fst h = fst hands then hands::t else h::(helper hands t) in
    helper hands racks

  let translate_coodinate (x,y) =
    (y, (Char.code (Char.lowercase_ascii x)  - Char.code 'a'))

  (* [update] is the new [state] resulting from evaluation of [move] in the
   * current [state] *)
  let update s m =
    match m with
    | Play {word = str; direction = dir; coordinate = crd}
      -> let rec helper str dir crd =
          match String.length str with
          | 0 -> ()
          | n -> let chr = Char.uppercase_ascii (String.get str 0) in
            let tile = get_tile crd s.board in
            let new_tile = match tile.letter with
            | Some c -> if c = chr then {bonus = tile.bonus; letter = Some chr}
              else failwith "You Cannot Override exsiting character"
            | None -> {bonus = tile.bonus; letter = Some chr} in
            fill_coordinate [crd] new_tile s.board;
            let next = get_nextcoodinate crd dir in
            helper (String.sub str 1 (String.length str - 1)) dir next in
        helper str dir (translate_coodinate crd);
      {
        board = s.board;
        score_board = s.score_board;
        letter_bag = s.letter_bag;
        player_racks = s.player_racks;
        turn = s.turn + 1;
        words = collect s.board
      }
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
        turn = s.turn + 1;
        words = s.words
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
        turn = s.turn + 1;
        words = s.words
      }
    | Pass ->
      {
        board = s.board;
        score_board = s.score_board;
        letter_bag = s.letter_bag;
        player_racks = s.player_racks;
        turn = s.turn + 1;
        words = s.words
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
        turn = s.turn;
        words = s.words
      }
end
