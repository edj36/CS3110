open Data
open Utils
open State
open Player

(*********** INITIALIZE PLAYERS ***********)

(* [get_players] is a list of players gathered from the user's string input*)
let rec get_players input_string_list =
  match input_string_list with
  |[]   -> []
  |h::[] -> failwith "Invalid Player Input"
  |h1::h2::t -> match h1 with
            |"Human" -> Human(h2) :: get_players t
            |"AI"    -> AI(h2)    :: get_players t
            | _      -> failwith "Invalid Player Input"



(*********** GUI ***********)

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


(*********** REPL ***********)

let rec repl c_state =
  let () = print_state c_state in
  let () = print_endline "\nEnter Move" in
  let s_move = read_line() in
  (* TODO AI vs Human input logic here *)
  let new_state = Human.execute_move s_move c_state in
  let () = print_endline "" in
  repl new_state

(*[init_repl] is the initial state created from the player list given by the
user's input*)
let initialize =
  let () = print_endline "Please Enter the Players and Names (i.e Human Eric)" in
  let s_players = read_line() in
  let split = Str.split (Str.regexp " +") (s_players ^ " ") in
  let player_list = get_players split in
  let init_state = setup player_list in
  let () = print_endline "" in
  repl init_state
