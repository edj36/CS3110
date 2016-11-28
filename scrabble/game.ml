open Data
open HumanMove
open Str
open Utils
open Yojson.Basic.Util

let open_json () = Yojson.Basic.from_file "info.json" 

let init_letter_bag src =
  let lb = src |> member "letter_bag" |> to_list in
  let chr = List.map (fun x -> x |> member "character" |> to_string) lb in
  let pt = List.map (fun x -> x |> member "pt" |> to_int) lb in
  let count = List.map (fun x -> x |> member "count" |> to_int) lb in
  let rec helper l1 l2 l3 =
    match l1, l2, l3 with
    | [],[],[] -> []
    | h1::t1, h2::t2, h3::t3 ->
      {character = String.get h1 0; pt = h2; count = h3} :: helper t1 t2 t3
    | _ -> failwith "list unbalanced" in
  helper chr pt count

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


(* pre-define players for the sake of testing *)
let players = [ Human "Alexis"; AI "Kenta" ]

(* [initialize_score] represents the tuple list of each player and their scores*)
let rec initialize_score (players : player list) =
  match players with
  | [] -> []
  | h::t -> (h, 0):: initialize_score t

(* [initilize_board] is a tile array array representation of game board *)
let initilize_board () =
  let board = Array.make_matrix 15 15 { bonus= Normal; letter = None } in
  let src = open_json () in
  fill_coordinate (init_tile src "Center")
    { bonus= Center; letter = None } board;
  fill_coordinate (init_tile src "Triple_word")
    { bonus= Triple_word; letter = None } board;
  fill_coordinate (init_tile src "Triple_letter")
    { bonus= Triple_letter; letter = None } board;
  fill_coordinate (init_tile src "Double_word")
    { bonus= Double_word; letter = None } board;
  fill_coordinate (init_tile src "Double_letter")
    { bonus= Double_letter; letter = None } board;
  board

(* [initialize_rack] is a (player * letter list) list, representating
 * each player's hands *)
let rec initialize_rack (players: player list) bag =
  match players with
  | []-> []
  | h::t -> let hand = draw_letters 7 bag in (h, hand) :: initialize_rack t bag

(* [initialize_state] is a representation of intial game state *)
let initialize_state (players: player list)=
  let src = open_json () in
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

(* [get_players] is a list of players gathered from the user's string input*)
let rec get_players input_string_list =
  match input_string_list with
  |[]   -> []
  |h::[] -> failwith "Invalid Player Input"
  |h1::h2::t -> match h1 with
            |"Human" -> Human(h2) :: get_players t
            |"AI"    -> AI(h2)    :: get_players t
            | _      -> failwith "Invalid Player Input"


let print_board state =
  print_string "\n";
  let b = state.board in
  for j = 0 to 14 do
    for i = 0 to 14 do
      let tile = get_tile (j,i) b in
      let chr = match tile.letter with
      | None -> " * "
      | Some c -> " " ^ Char.escaped c ^ " " in
      print_string chr;
    done;
    print_string "\n";
  done


let print_state state =
  print_string ("Turn: " ^ (string_of_int state.turn) ^ "\n");
  let player = current_player state in
  let name = match fst player with
  | Human n1 -> n1
  | AI n2 -> n2 in
  print_string (name ^ "\n");
  let lst = List.map (fun x -> Char.escaped x.character) (snd player) in
  let rec helper = function
  | [] -> ""
  | h::t -> h^ " " ^ helper t in
  let hands = helper lst in
  let () = print_string ("Player's hand: " ^ hands ^ "\n") in
  let () = print_board state in
  let () = List.fold_left (fun acc elm -> print_string (elm ^ "\n")) () state.words in
  let () = print_string "\nnew words \n" in
  let () = List.fold_left (fun acc elm -> print_string (elm ^ "\n")) () (get_newwords state)
  in ()


let rec repl c_state =
  let () = print_state c_state in
  let () = print_endline "\nEnter Move" in
  let s_move = read_line() in
  let new_state = HumanMove.submit_move c_state (HumanMove.get_move s_move) in
  let () = print_endline "" in
  repl new_state

let rec main_repl c_state =
  let () = print_endline "Enter Move" in
  let s_move = read_line() in
  let new_state = HumanMove.submit_move c_state (HumanMove.get_move s_move) in
  let () = print_endline "" in
  repl new_state

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
