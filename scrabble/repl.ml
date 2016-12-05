open Data
open Utils
open Player
open Yojson.Basic.Util
open State
open Gui

(********** REPL **********)

(* [is_tie] : bool
 * true indicating that score_board shows it is a tie game
 * false indicates there is a winner *)
let is_tie lst =
  let scores = List.map (fun x -> snd x ) lst in
  match scores with
  | [] -> false
  | h::t -> List.mem h t

(* [get_players] : string -> player list
 * waits for a string input and create a list of players
 * the string must satisfy some rules:
 * 1, [Human name] for human player
 * 2, [AI name level] for AI
 * 3, cannot have duplicate names
 * 4, number of player must be less than 5 (4 max)
 * 5, Ai's level must be 1 ~ 7
 * raise exception if these rules are not satisified *)
let rec get_players input_string_list =
  let rec helper_get_players = function
  | []   -> []
  | h::[] -> failwith "Invalid Player Input"
  | h1::h2::h3::t -> (match String.lowercase_ascii h1 with
    | "human" | "h" -> Human (h2) :: helper_get_players (h3::t)
    | "ai" | "a"    -> let level = int_of_string h3 in
    if level < 1 || level > 7 then raise Error_ai_level else
      AI (h2, level) :: helper_get_players (t)
    | _      -> failwith "Invalid Player Input")
  | h1 :: h2 :: t -> (match String.lowercase_ascii h1 with
    | "human" | "h" -> Human (h2) :: helper_get_players t
    | _      -> failwith "Invalid Player Input") in
  let lst = match List.length (helper_get_players input_string_list) with
    | 1 | 2 | 3 | 4 -> helper_get_players input_string_list
    | _ -> raise Error_too_many_players in
  let names = List.map (fun x -> match x with Human n1->n1 | AI (n2,i)->n2) lst in
  let rec check_duplicate = function
    | [] -> true
    | h :: t -> (not (List.mem h t)) && (check_duplicate t) in
  if check_duplicate names then lst else raise Error_duplicate_names

(* [repl] : state -> state
 * [repl] is a main repl of the game. First evaluate if the game is ended.
 * When swapall is used 15 times consecutively, or quit command is used, or
 * swapall or swapsome is used when bag is empty, game will end.
 * If game ends,
 Evaluate the currrent player and call
 * function in Player module. *)
let rec repl c_state : Data.game_state =
  if 15 = c_state.counter || c_state.quit
  then end_game c_state
  else let pl = fst (current_player_rack c_state) in
  let () = update_gui c_state in
  let new_state = match pl with
    | AI n -> AI.execute_move c_state c_state
    | Human n ->
      let () = print_string "\nEnter Move\n> " in
      let s_move = read_line() in
      try Human.execute_move s_move c_state with
      | Error_not_center -> print_message "error_not_center"; c_state
      | Error_not_have -> print_message "error_not_have"; c_state
      | Error_not_fit -> print_message "error_word_not_fit"; c_state
      | Error_not_touching -> print_message "error_not_touching"; c_state
      | Error_not_in_dictionary -> print_message "error_not_in_dictionary"; c_state
      | Error_existing_letter -> print_message "error_existing_letter"; c_state
      | _ -> let () = print_endline "Invalid command" in c_state in
  let _ = print_endline "" in
  repl new_state

(* [end_game] : state -> state
 * Displays a window when game is ended. Waits for the next command. Player can
 * either start the new game, or quit the game.
 * The window will evaluate the game and check if there is a winner or tie game.
 * Sort the player by the decending order of their score, and display them *)
and end_game state =
  let help_sort elm1 elm2= Pervasives.compare (snd (elm1)) (snd (elm2)) in
  let winner_list = List.rev (List.sort help_sort state.score_board) in
  let winner = match winner_list with
   | [] -> failwith "No Player"
   | h::t -> (match fst h with Human n1 -> n1 | AI (n2,i) -> n2) in
  let () = if is_tie winner_list then
  ANSITerminal.print_string [ANSITerminal.magenta] ("TIE GAME!\n")
  else ANSITerminal.print_string [ANSITerminal.magenta]
    ("COGRATULATIONS " ^ String.uppercase_ascii winner ^ "!\n\n") in
  print_score winner_list;
  print_string "\n* New Game?  --> [play / p]";
  print_string "\n* Quit Game? --> [quit / q]\n\n> ";
  let rec helper str =
    match String.trim (String.lowercase_ascii str) with
    | "play" | "p" -> print_string "Please type [play / p] or [quit / q]\n\n> ";
      initialize_game ()
    | "quit" | "q" -> print_string "Thank you for playing!\n\n"; state
    | _ -> print_string "Please type [play / p] or [quit / q]\n\n> ";
      helper (read_line ()) in
  helper (read_line())

(* [initialize_game] : unit -> unit
 * [initialize_game] waits for valid inputs and enters main REPL.
 * Waits for the input to create a player list, catches exceptions
 * if there are invalid inputs from users.
 * Initializes state and enter main repl *)
and initialize_game () =
   ANSITerminal.print_string [ANSITerminal.green]
   "\n\nPlease Enter the Players and Names (4 Players Max)\n\n";
   print_endline "* Type [Human Eric] or [H Eric] for human player";
   print_endline
   "* Type [AI Kenta 5] or [A Kenta 5] for level 5 AI player (level: 1 ~ 7)";
   let () = print_string
   "(Please do not choose same name for multiple players)\n\n> " in
   let rec helper str =
      let split = Str.split (Str.regexp " +") (str ^ " ") in
      let player_list = try get_players split with
       | Error_duplicate_names -> print_message "error_duplicate_names"; []
       | Error_too_many_players -> print_message "error_too_many_players"; []
       | Error_ai_level -> print_message "error_ai_level"; []
       | Failure _ -> print_string "Invalid players"; [] in
        match player_list with
        | [] -> print_string "\n\n> "; helper (read_line ())
        | _ -> repl (setup player_list) in
    helper (read_line ())

(* [main_manu] : unit -> unit
 * [main_manu] displays a main manu of the game. Waits for the string input
 * and moves on to next window based on the input
 * [play] -> call initialize_game
 * [help] -> displays tutorial and waits for the next string
 * [quit] -> quit the game *)
let main_menu () =
  ANSITerminal.print_string [ANSITerminal.magenta]
    ("\n=== WELCOME TO MULTIPLAYER SCRABBLE! ===\n\n");
  print_string "\n* Tutorial?  --> [help / h]";
  print_string "\n* New Game?  --> [play / p]";
  print_string "\n* Quit Game? --> [quit / q]\n\n> ";
  let rec helper2 str =
  match String.trim (String.lowercase_ascii str) with
    | "quit" | "q"-> print_string "Thank you for playing!\n\n";
    | "play" | "p"-> (fun x -> ()) (initialize_game ())
    | "help" | "h"-> print_message "tutorial";
    print_string "Please type [play / p] or [quit /q]\n\n> ";
      helper2 (read_line ())
    | _ -> print_string "Please type correct command\n\n> "; helper2 (read_line ()) in
  helper2 (read_line())
