open Data
open String
open Str
open List
open API

module type Move = sig
  type state
  type move
  val get_move : string -> move
  val submit_move: state -> move -> state
  val validate : string -> bool
end

module HumanMove : (Move with type state = game_state) =  struct

  (* type for game state *)
  type state = game_state

  (* type for the move player makes *)
  type move = moves

  (* [string_to_direction] is a type direction representation
   * of string type input [s] *)
  let string_to_direction s =
    match s with
    | "Across" | "a" | "across"-> Across
    | "Down" | "d" | "down" -> Down
    | _ -> failwith "Invaild direction"

  (* [check_coordinate] is a bool indicating the validity of input coordinate
   * false means the invalid input for the coodinate, true mweans the coodinate
   * is valid *)
  let check_coordinate (x,y) =
    let lower_x = Char.lowercase_ascii x in
     (0 <= y) && (y <= 14)
     && (Char.code lower_x >= Char.code 'a')
     && (Char.code 'o' >= Char.code lower_x)
     
  let get_move s =
    let split = Str.split (Str.regexp " +") (s ^ " ") in
    let move = List.nth split 0 in
    let n = List.length split in
    match move with
    | "Play" | "play" | "p" ->
      let coordinate = (String.get (List.nth split 3) 0,
      (int_of_string (List.nth split 4))) in
      if n = 5 && (check_coordinate coordinate) then
      Play
      {
        word = List.nth split 1;
        direction = string_to_direction (List.nth split 2);
        coordinate = coordinate
      }
      else failwith "Invalid coordinate"
    | "SwitchAll" | "Switch_All" | "sa" | "s_a" ->
      if n = 1 then SwitchAll
      else failwith "Invalid command"
    | "SwitchSome" | "switchsome" | "s" | "s_s"->
      if n >= 2 then
        let char_lst = List.map (fun x -> String.get x 0) (List.tl split) in
        SwitchSome char_lst
      else failwith "Invalid command"
    | "Pass" | "pass" -> if n = 1 then Pass else failwith "Invalid command"
    | "Shuffle" | "shuffle" -> if n = 1 then Shuffle else failwith "Invalid command"
    | _ -> failwith "Invalid command"

  let validate s = failwith "Unimplemented"

  (* [update_racks] is an updated player_rack list after substituting the old
   * player_rack with new [hands] *)
  let update_racks hands s =
    let racks = s.player_racks in
    let rec helper hands racks =
    match racks with
    | [] -> []
    | h::t -> if fst h = fst hands then hands::t else h::(helper hands t) in
    helper hands racks

  (* [translate_coodinate] is an int*int representation of char*int coordinate*)
  let translate_coodinate (x,y) =
    (y-1, (Char.code (Char.lowercase_ascii x)  - Char.code 'a'))

  (* [submit_move] enters [move] to the game and is the [state] resulting from
   * [move]'s' execution *)
  let submit_move (s :state) (m:move) =
    match m with
    | Play {word = str; direction = dir; coordinate = crd}
      ->
      let rec helper str dir crd =
        match String.length str with
        | 0 -> ()
        | n ->
        let chr = Char.uppercase_ascii (String.get str 0) in
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
        turn = s.turn + 1
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
