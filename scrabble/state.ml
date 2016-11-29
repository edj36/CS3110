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
let board = Array.make_matrix 15 15 { bonus = Normal ; letter = None } in
let src = Yojson.Basic.from_file "info.json" in
let () = fill_coordinate (init_tile src "Center")
  { bonus= Center; letter = None }  board in
let () = fill_coordinate (init_tile src "Triple_word")
  { bonus= Triple_word; letter = None } board in
let () = fill_coordinate (init_tile src "Triple_letter")
  { bonus= Triple_letter; letter = None } board in
let () = fill_coordinate (init_tile src "Double_word")
  { bonus= Double_word; letter = None } board in
let () = fill_coordinate (init_tile src "Double_letter")
  { bonus= Double_letter; letter = None } board in
board

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
    | (x,y)::t -> if player = x then (x, pt) :: t
    else (x,y) :: helper pt player t in
  helper pt player state.score_board

(********** UPDATE **********)

(* [submit_move] enters [move] to the game and is the [state] resulting from
 * [move]'s' execution *)
let update m s = match m with
  | Play {word = str; direction = dir; coordinate = crd} ->
    let prev_words = collect s.board in
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
        let next = try get_nextcoordinate crd dir with
      | Failure _ -> (15,15) in
        if next = (15,15) then ()
        else helper (String.sub str 1 (String.length str - 1)) dir next in
    helper str dir (translate_coodinate crd);

    (*score*)
    let new_words = get_newwords (collect s.board) s.words in
    let basic_score = List.fold_left (fun a e -> a + (word_score e s)) 0 new_words in
    let new_scoreboard = update_scoreboard basic_score s in
    let temp = update_switch_some (string_to_char_list str) s in
    let new_racks = temp.player_racks in
    {
      board = s.board;
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
    Random.self_init();
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
  | _ -> failwith "never happens"
