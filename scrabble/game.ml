open Data
open Utils
open State
open Player
open Gui

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

(*********** REPL ***********)

(* [repl] main repl *)
let rec repl c_state : Data.game_state =
  let pl = fst (current_player_rack c_state) in 
  let () = update_gui c_state in
  let new_state = match pl with 
    | AI n -> AI.execute_move c_state c_state
    | Human n ->
      let () = print_endline "\nEnter Move" in
      let s_move = read_line() in
      Human.execute_move s_move c_state in
  let () = print_endline "" in
  repl new_state

(*[initialize] is the initial state created from the player list given by the
user's input*)
let initialize =
  let () = print_endline "Please Enter the Players and Names (i.e Human Eric)" in
  let s_players = read_line() in
  let split = Str.split (Str.regexp " +") (s_players ^ " ") in
  let player_list = get_players split in
  let init_state = setup player_list in
  let () = print_endline "" in
  repl init_state
