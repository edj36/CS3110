open Data
open Utils
open Yojson.Basic.Util

(********** JASON PARSER **********)

(* [init_letter] is a letter list representation of intial letter bag *)
let init_letter_bag src =
  let lb = src |> member "letter_bag" |> to_list in
  let chr = List.map (fun x -> x |> member "character" |> to_string) lb in
  let pt = List.map (fun x -> x |> member "pt" |> to_int) lb in
  let count = List.map (fun x -> x |> member "count" |> to_int) lb in
  let rec helper l1 l2 l3 =
    match l1, l2, l3 with
    | [],[],[] -> []
    | h1::t1, h2::t2, h3::t3 ->
      {
        character = (String.get h1 0);
        pt = h2; count = h3
      } :: helper t1 t2 t3
      | _ -> failwith "list unbalanced" in
    helper chr pt count

(* [init_tile] is a (int*int) list representation of coordintes for the
 * specified bonus tile type*)
let init_tile src name =
  let tile = src |> member name |> to_list in
  let x = List.map (fun x -> x |> member "x" |> to_int) tile in
  let y = List.map (fun x -> x |> member "y" |> to_int) tile in
  let rec helper l1 l2 =
    match l1, l2 with
    | [],[] -> []
    | h1::t1, h2::t2 -> (h1,h2) :: helper t1 t2
    | _ -> failwith "list unbalanced" in
  helper x y

(********** INITIALIZE STATE **********)

(* [initialize_score] represents the tuple list of each player and their scores *)
let rec initialize_score (players : player list) =
  match players with
  | [] -> []
  | h::t -> (h, 0):: initialize_score t

(* [initilize_board] is a tile array array representation of game board *)
let initilize_board () =
  let board =
    let rec helper i fill =
      if i = 0 then [] else fill :: helper (i-1) fill in
    helper 15 (helper 15 { bonus = Normal ; letter = None }) in
  let src = Yojson.Basic.from_file "info.json" in
  let board1 = fill_coordinate (init_tile src "Center")
    { bonus= Center; letter = None }  board in
  let board2 = fill_coordinate (init_tile src "Triple_word")
    { bonus= Triple_word; letter = None } board1 in
  let board3 = fill_coordinate (init_tile src "Triple_letter")
    { bonus= Triple_letter; letter = None } board2 in
  let board4 = fill_coordinate (init_tile src "Double_word")
    { bonus= Double_word; letter = None } board3 in
  let board5 = fill_coordinate (init_tile src "Double_letter")
    { bonus= Double_letter; letter = None } board4 in
  board5

(* [initialize_rack] is a (player * letter list) list, representating
* each player's hands *)
let rec initialize_rack (players: player list) bag =
  match players with
  | []-> []
  | h::t -> let hand = draw_letters 7 bag in (h, hand) :: initialize_rack t bag

(********** SETUP **********)

(* [initialize_state] is a representation of intial game state *)
let setup players =
  let src = Yojson.Basic.from_file "info.json" in
  let initial_score = initialize_score players in
  let initial_board = initilize_board () in
  let initial_bag = init_letter_bag src in
  let racks = initialize_rack players initial_bag in
  {
    board = initial_board;
    score_board = initial_score;
    letter_bag = initial_bag;
    player_racks = racks;
    turn = 0;
    words = []
  }


(********** UPDATE STATE **********)

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
  (y-1, (Char.code (Char.lowercase_ascii x) - Char.code 'a'))

(* [update_switch_all] is a new type game_state after executing
 * switch all letters *)
let update_switch_all state =
  let player = current_player_rack state in
  add_letter (snd player) state.letter_bag;
  let new_hand = (fst player, draw_letters 7 state.letter_bag) in
  let new_racks = update_racks new_hand state in
  {
    board = state.board;
    score_board = state.score_board;
    letter_bag = state.letter_bag;
    player_racks = new_racks;
    turn = state.turn + 1;
    words = state.words
  }

(* [update_switch_some] is a new type game_state after executing
 * switch some letters represented by char list *)
let update_switch_some lst state =
  let letters = List.map (fun x -> char_to_letter x state.letter_bag) lst in
  let player = current_player_rack state in
  add_letter letters state.letter_bag;
  let removed =
    let rec helper letter hands = match letter with
      | [] -> hands
      | h::t -> helper t (remove h hands)
    in helper letters (snd player) in
  let new_hand =
  (fst player, removed @ (draw_letters (List.length letters) state.letter_bag)) in
  let new_racks = update_racks new_hand state in
  {
    board = state.board;
    score_board = state.score_board;
    letter_bag = state.letter_bag;
    player_racks = new_racks;
    turn = state.turn + 1;
    words = state.words
  }

(********** SCORING **********)

(*[update_scoreboard] is an updated score_board after substituting the old
 * score_board with the current player's score *)
let update_scoreboard pt state =
  let player = fst (current_player_rack state) in
  let rec helper pt player score_board =
    match score_board with
    | [] -> []
    | (x,y)::t -> if player = x then (x, y + pt) :: t
    else (x,y) :: helper pt player t in
  helper pt player state.score_board

(* [collect_words_on_crd] *)
let collect_words_on_crd crd state =
  let init = get_tile crd state.board in
  let init_pt = match init.letter with
  | None -> 0
  | Some c -> let letter = char_to_letter c state.letter_bag in letter.pt in
  let rec helper (x,y) state dir delta =
    let tile = get_tile (x,y) state.board in
    match tile.letter with
    | None -> 0
    | Some i -> let next = match dir with
      | Across -> (x, y + delta)
      | Down -> (x + delta, y) in
      let l = char_to_letter i state.letter_bag in
      if fst next > -1 && fst next < 15 && snd next > -1 && snd next < 15 then
        l.pt + (helper next state dir delta)
      else l.pt in
  (helper crd state Across 1) + (helper crd state Across (-1)) +
  (helper crd state Down 1) + (helper crd state Down (-1)) - 3 * init_pt


(* [bonus_score] is an int representation of all bonus points collected from
 * the list of coordinates *)
let rec bonus_score crds state =
  match crds with
  | [] -> 0
  | h::t -> let tile = get_tile h state.board in
  match tile.bonus with
  | Double_letter -> (match tile.letter with
    |Some c -> let l = (char_to_letter c state.letter_bag) in
      2 * l.pt + bonus_score t state
    |None -> failwith "never happens")
  | Double_word -> 2 * (collect_words_on_crd h state) + bonus_score t state
  | Triple_letter -> (match tile.letter with
    |Some c -> let l = (char_to_letter c state.letter_bag) in
      3 * l.pt + bonus_score t state
    |None -> failwith "never happens")
  | Triple_word -> 3 * (collect_words_on_crd h state) + bonus_score t state
  | Center -> bonus_score t state
  | Normal -> bonus_score t state

(********** UPDATE **********)

(* [submit_move] enters [move] to the game and is the [state] resulting from
 * [move]'s' execution *)
let update m s = match m with
  | Play {word = str; direction = dir; coordinate = crd} ->
    let prev_words = collect s.board in
    let prev_crds = collect_coordinates s.board in
    let rec helper str dir crd board =
      match String.length str with
      | 0 -> board
      | n ->
        let chr = Char.uppercase_ascii (String.get str 0) in
        let tile = get_tile crd board in
        let new_tile = match tile.letter with
          | Some c -> if c = chr then {bonus = tile.bonus; letter = Some chr}
            else failwith "You Cannot Override exsiting character"
          | None -> {bonus = tile.bonus; letter = Some chr} in
            let update = fill_coordinate [crd] new_tile board in
        let next = try get_nextcoordinate crd dir with
          | Failure _ -> (15,15) in
        if next = (15,15) then board
        else helper (String.sub str 1 (String.length str - 1)) dir next update in
    let new_board = helper str dir (translate_coodinate crd) s.board in
    (*score*)
    let new_words = get_newwords (collect new_board) s.words in
    let new_crds = get_newcoordinates (collect_coordinates new_board) prev_crds in
    let basic_score = List.fold_left (fun a e -> a + (word_score e s)) 0 new_words in
    let bonus_score = bonus_score new_crds s in
    let new_scoreboard = update_scoreboard (basic_score + bonus_score) s in
    let temp = update_switch_some (string_to_char_list str) s in
    let new_racks = temp.player_racks in
    {
      board = new_board;
      score_board = new_scoreboard;
      letter_bag = s.letter_bag;
      player_racks = new_racks;
      turn = s.turn + 1;
      words = prev_words
    }
  | SwitchAll -> update_switch_all s
  | SwitchSome lst -> update_switch_some lst s
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
    let player = current_player_rack s in
    let shuffled = shuffle (snd player) in
    let new_racks = update_racks ((fst player), shuffled) s in
    {
      board = s.board;
      score_board = s.score_board;
      letter_bag = s.letter_bag;
      player_racks = new_racks;
      turn = s.turn;
      words = s.words
    }
  | _ -> failwith "never happens"
