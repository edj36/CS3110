open Data

let sample_players = [Human "Eric J"; Human "Eric Z"; AI "Pehuen"; AI "Kenta"]

let initialize_bag ()=
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

let rec initialize_score = function
  | [] -> []
  | h::t -> (h, 0):: initialize_score t

(* [draw_char] is a letter list representing the state
 * after drawing a letter from the list *)
let rec draw_char c bag =
  match bag with
  | []-> ()
  | h::t ->
  if h.character = c then h.count <- h.count - 1
  else draw_char c t

(* [rand_char] is a letter option which is simulated after randomly pick
 * one letter from the letter bag *)
let rand_char bag =
  let sum = List.fold_left (fun acc elm -> acc + elm.count) 0 bag in
  let num = Random.int sum in
  let rec helper num lst = match lst with
  | [] -> None
  | h :: t -> if num <= h.count then Some h
  else helper (num-h.count) t in helper num bag

let rec draw_letters num bag =
  match num with
  | 0 -> []
  | _ -> let l = rand_char bag in
  (match l with
  | None -> failwith "Bag is enpty";
  | Some l -> draw_char l.character bag); l::draw_letters (num-1) bag

let sample_board () =
  let board = Array.make_matrix 15 15 { bonus= Normal; letter = None } in
  board.(7).(7) <- { bonus= Center; letter = None }; board

let rec initialize_rack players bag =
  match players with
  | []-> []
  | h::t -> let hand = draw_letters 7 bag in (h, hand) :: initialize_rack t bag

let initialize_state (players: player list)=
  let initial_score = initialize_score players in
  let initial_board = sample_board () in
  let initial_bag = initialize_bag () in
  let racks = initialize_rack players initial_bag in
  {
    board = initial_board;
    score_board = initial_score;
    letter_bag = initial_bag;
    player_racks = racks;
    turn = 0
  }
