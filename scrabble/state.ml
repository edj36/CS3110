open Data
open Utils
open Yojson.Basic.Util

(********** JASON PARSER **********)

(* [init_letter] is a letter list representation of intial letter bag
 * Parse information stored in a Json file and creates a letter bag *)
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
        character = String.make 1 (String.get h1 0);
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

(********** UPDATE LETTER BAG **********)

(* [add_or_draw_char] is a letter list representing the state
* after adding or drawing a letter from the list
* [op] : either (+) or (-) *)
let rec add_or_draw_char c bag op =
  match bag with
  | []-> ()
  | h::t ->
  if h.character = c then h.count <- op h.count 1
  else add_or_draw_char c t op

(* [rand_char] is a letter option which is simulated after randomly pick
* one letter from the letter bag *)
let rand_char bag =
  let sum = List.fold_left (fun acc elm -> acc + elm.count) 0 bag in
  match sum with
  | 0 -> failwith "Bag is empty!"
  | _ ->
  Random.self_init ();
  let num = Random.int sum in
  let rec helper num lst = match lst with
  | [] -> None
  | h :: t -> if (num <= h.count) && (h.count <> 0) then Some h
  else helper (num-h.count) t in helper num bag

(* [draw_letters] represents the letter list after drawing specified
* number of letters from bag. *)
let rec draw_letters num bag =
  let sum = List.fold_left (fun a e -> a + e.count) 0 bag in
  if sum >= num then
    match num with
    | 0 -> []
    | _ -> let l = rand_char bag in
    (match l with
    | None -> failwith "Bag is enpty";
    | Some l -> add_or_draw_char l.character bag (-);
    l::draw_letters (num-1) bag)
  else draw_letters sum bag

(* [add_letters] represents the letter list after adding the letters to the list *)
let rec add_letter hands bag =
  match hands with
  | [] -> ()
  | h :: t -> add_or_draw_char h.character bag (+); add_letter t bag

(* [remove_letters] represents unit type produced after updating mutable field
 * in letter bag *)
let remove_string str bag =
  let rec helper lst bag =
    match lst with
      | [] -> ()
      | h::t -> add_or_draw_char h bag (-); helper t bag in
  helper (string_to_char_list str) bag

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
let empty_state = {
  board = initilize_board ();
  score_board = [];
  letter_bag = [];
  player_racks = [];
  turn = 0;
  counter = 0;
  quit = false
}

(* [initialize_state] is a type game_state representation of intial game state *)
let setup players =
  let src = Yojson.Basic.from_file "info.json" in
  let initial_bag = init_letter_bag src in
  {
    board = initilize_board ();
    score_board = initialize_score players;
    letter_bag = initial_bag;
    player_racks = initialize_rack players initial_bag;
    turn = 0;
    counter = 0;
    quit = false
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

(* [update_switch_all] is a new type game_state after executing
 * switch all letters *)
let update_switch_all state =
  let player = current_player_rack state in
  add_letter (snd player) state.letter_bag;
  let new_hand = (fst player, draw_letters 7 state.letter_bag) in
  {
    board = state.board;
    score_board = state.score_board;
    letter_bag = state.letter_bag;
    player_racks = update_racks new_hand state;
    turn = state.turn + 1;
    counter = state.counter + 1;
    quit = ((List.fold_left (fun acc e -> acc + e.count) 0 state.letter_bag) = 0)
  }

(* [update_switch_some] is a new type game_state after executing
 * switch some letters represented by char list *)
let update_switch_some lst state =
  let letters = List.map
    (fun x -> char_to_letter (String.uppercase_ascii x) state.letter_bag) lst in
  let player = current_player_rack state in
  add_letter letters state.letter_bag;
  let removed =
    let rec helper letter hands = match letter with
      | [] -> hands
      | h::t -> helper t (remove h hands)
    in helper letters (snd player) in
  let new_hand =
  (fst player, shuffle (removed @ (draw_letters (List.length letters) state.letter_bag))) in
  {
    board = state.board;
    score_board = state.score_board;
    letter_bag = state.letter_bag;
    player_racks = update_racks new_hand state;
    turn = state.turn + 1;
    counter = state.counter + 1;
    quit = false
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

(* [collect_words_on_crd] is an int representation of scire *)
let collect_words_on_crd crd board letter_bag=
  let init = get_tile crd board in
  let init_pt = match init.letter with
  | None -> 0
  | Some c -> let letter = char_to_letter c letter_bag in letter.pt in
  let rec helper (x,y) board dir delta =
    let tile = get_tile (x,y) board in
    match tile.letter with
    | None -> 0
    | Some i -> let next = match dir with
      | Across -> (x, y + delta)
      | Down -> (x + delta, y) in
      let l = char_to_letter i letter_bag in
      if fst next > -1 && fst next < 15 && snd next > -1 && snd next < 15 then
        l.pt + (helper next board dir delta)
      else l.pt in
  (helper crd board Across 1) + (helper crd board Across (-1)) +
  (helper crd board Down 1) + (helper crd board Down (-1)) - 3 * init_pt

(* [bonus_score] is an int representation of all bonus points collected from
 * the list of coordinates *)
let rec bonus_score crds board letter_bag=
  match crds with
  | [] -> 0
  | h::t -> let tile = get_tile h board in
  match tile.bonus with
  | Double_letter -> (match tile.letter with
    |Some c -> let l = (char_to_letter c letter_bag) in
      2 * l.pt + bonus_score t board letter_bag
    |None -> failwith "never happens")
  | Double_word -> 2 * (collect_words_on_crd h board letter_bag)
    + bonus_score t board letter_bag
  | Triple_letter -> (match tile.letter with
    |Some c -> let l = (char_to_letter c letter_bag) in
      3 * l.pt + bonus_score t board letter_bag
    |None -> failwith "never happens")
  | Triple_word -> 3 * (collect_words_on_crd h board letter_bag)
    + bonus_score t board letter_bag
  | Center -> bonus_score t board letter_bag
  | Normal -> bonus_score t board letter_bag

(* [update_score] calculates the result score of previous move *)
let update_score old_w old_c new_board state =
  let new_w = get_newwords (collect new_board) old_w in
  let new_c = get_newcoordinates (collect_coordinates new_board) old_c in
  let basic = List.fold_left (fun a e -> a + (word_score e state)) 0 new_w in
  let bonus = bonus_score new_c new_board state.letter_bag in
  bonus + basic

(********** UPDATE **********)

(* [submit_move] enters [move] to the game and is the [state] resulting from
 * [move]'s' execution *)
let update m s = match m with
  | Play {word = str; direction = dir; coordinate = crd} ->
    let prev_words = collect s.board in
    let prev_crds = collect_coordinates s.board in
    let new_board = place_string str dir (translate_coodinate crd) s.board in
    let chrlst =
      get_newletters (get_newcoordinates
        (collect_coordinates new_board) prev_crds) new_board in
    let l_played = List.fold_left (fun a e -> a ^ e) "" chrlst in
    remove_string l_played s.letter_bag;
    (** score **)
    let score = update_score prev_words prev_crds new_board s in
    let new_scoreboard = update_scoreboard score s in
    let temp = update_switch_some (string_to_char_list l_played) s in
    let new_racks = temp.player_racks in
    {
      board = new_board;
      score_board = new_scoreboard;
      letter_bag = s.letter_bag;
      player_racks = new_racks;
      turn = s.turn + 1;
      counter = 0;
      quit = false
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
      counter = s.counter;
      quit = false
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
      counter = s.counter;
      quit = false
    }
  | End ->   {
      board = s.board;
      score_board = s.score_board;
      letter_bag = s.letter_bag;
      player_racks = s.player_racks;
      turn = s.turn;
      counter = s.counter;
      quit = true
    }
