open Data
open HumanMove
open Str


(* pre-define players for the sake of testing *)
let players = [ Human "Alexis"; AI "Kenta" ]

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


(* [draw_char] is a letter list representing the state
 * after drawing a letter from the list *)
let rec draw_char c bag =
  match bag with
  |[]-> bag
  |h::t ->
  if h.character = c then let () = h.count <- (h.count - 1) in bag
  else h :: draw_char c t

(* [rand_char] is a letter option which is simulated after randomly pick
 * one letter from the letter bag *)
let rand_char bag =
  let sum = List.fold_left (fun acc elm -> acc + elm.count) 0 bag in
  let num = Random.int sum in
  let rec helper num lst = match lst with
  | [] -> None
  | h :: t -> if num <= h.count then Some h else helper (num-h.count) t
  in helper num bag

(* [draw_letters] represents the letter option list after drawing specified
 * number of letters from bag. *)
let rec draw_letters num bag =
  match num with
  | 0 -> []
  | _ -> let l = rand_char bag in
  (match l with
  | None -> failwith "Bag is enpty";
  | Some l -> draw_char l.character bag); l::draw_letters (num-1) bag


let initial_hands (players : player list)=
  let initial_bag = letter_bag () in
  let rec player_hand player bag =
    match players with
    | []-> []
    | h::t->
    let rec helper num bag =
      if num = 0 then []
      else match (rand_char bag) with
        | None -> []
        | Some h -> let new_bag = draw_char h.character bag in
        h :: helper (num-1) new_bag in
    let hand = helper 7 bag in
    let new_bag =
    List.fold_left (fun acc elm -> acc |> draw_char elm.character) bag hand in
  [(h,hand)] :: player_hand t new_bag in
  player_hand players initial_bag


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

(* [fill_coodinate] is an updated game board after filling the specified
 * coordinates with element, [fill] *)
let fill_coordinate coordinates fill board =
  match coordinates with
  |[]-> ()
  |(x,y)::t -> board.(x).(y)<- fill

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

(* [initialize_state] is a representation of intial game state *)
let initialize_state (players: player list)=
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

(* [get_players] is a list of players gathered from the user's string input*)
let rec get_players input_string_list =
  match input_string_list with
  |[]   -> []
  |h::[] -> failwith "Invalid Player Input"
  |h1::h2::t -> match h1 with
            |"Human" -> Human(h2) :: get_players t
            |"AI"    -> AI(h2)    :: get_players t
            | _      -> failwith "Invalid Player Input"


(*[init_repl] is the initial state created from the player list given by the
user's input*)
let init_repl =
  let () = print_endline "Please Enter the Players and Names (i.e Human Eric)" in
  let s_players = read_line() in
  let split = Str.split (Str.regexp " +") (s_players ^ " ") in
  let player_list = get_players split in
  let init_state = initialize_state player_list in
  let () = print_endline "" in
  main_repl init_state


let rec main_repl c_state =
  let () = print_endline "Enter Move" in
  let s_move = read_line() in
  let new_state = HumanMove.submit_move c_state (HumanMove.get_move s_move) in
  let () = print_endline "" in
  repl new_state