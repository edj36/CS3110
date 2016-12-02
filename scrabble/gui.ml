open Data
open Utils
open Player
open State
open Yojson.Basic.Util

exception Error_existing_letter
exception Error_not_fit
exception Error_not_center
exception Error_not_in_dictionary
exception Error_not_have
exception Error_not_touching
exception Invalid

(*********** GUI ***********)

(* [y_axis] is a string type representation of y_axis labeling of the scrabble
 * board *)
let y_axis num =
  if (num+1) > 9 then string_of_int (num+1) else " " ^ (string_of_int (num+1))

(* [print_board] is a unit type, side effect of print_stirng functions.
 * [print_board] searches through the index of game board, color tiles if there
 * is a bonus tile, print alphabet if the tile is occupied.
 * [state]: Data.board of current state *)
let print_board state =
  ANSITerminal.(print_string [] "\n";
  let src = Yojson.Basic.from_file "info.json" in
  let y_index = src |> member "y_index" |> to_string in
  let b = state.board in
  print_string [white] y_index;
  for j = 0 to 14 do
  print_string [white] (y_axis j);
    for i = 0 to 14 do
      let tile = get_tile (j,i) b in
      let (lst, str) = match tile.letter with
      | None -> ( match tile.bonus with
        | Double_letter -> ([on_cyan; black], " DL ")
        | Double_word -> ([on_blue; white], " DW ")
        | Triple_letter -> ([on_green; black], " TL ")
        | Triple_word -> ([on_red; black], " TW ")
        | Center -> ([on_magenta; white], " CE ")
        | Normal -> ([on_white; black], "    " ) )
      | Some c -> ([on_yellow; black], " " ^ Char.escaped c ^ "  ") in
      print_string [] " "; print_string lst str; print_string [] " ";
    done;
    print_string [Reset] "\n \n";
  done )

(* [concat_space] is a string after prepending specified number of spaces
 * to the string
 * [num]: int, number of spaces
 * [str]: string, input string *)
let rec concat_space num str =
  if num = 0 then str else concat_space (num-1) (" " ^ str)

(* [print_score] is a string type of score board. Take a current score_board
 * and prints player's name followed by the score
 * input: game.score_board *)
let rec print_score = function
  | []-> ()
  | (x,y)::t -> let name = (match x with
    |Human n1 -> n1
    |AI n2 -> n2) in
  print_string (name ^ ": " ^ (string_of_int y) ^ " \n");
  print_score t

(* [print_message] will parse json file and print message stoared in a json
 * file.
 * [name] : string, name of json member *)
let print_message name =
  let src = Yojson.Basic.from_file "info.json" in
  let message = src |> member name |> to_string in
  print_string message

(* [update_gui] Data.game_state -> unit
 * prints current state on terminal. take current game state and check following
 * information.
 * 1, Current turn (adds 1 before printing because turn starts from 0 )
 * 2, Current player's name
 * 3, Total number of letters left in the bag
 * 4, Print current game board
 * 5, Print command rule
 * 6, print score board
 * 7, player's hand *)
let update_gui state =
  (* 1 TURN *)
  print_string ("\n\nTurn: " ^ (string_of_int (state.turn + 1)) ^ "\n");
  (* 2 NAME OF PLAYER *)
  let player = current_player_rack state in
  let name = match fst player with
    | Human n1 -> n1
    | AI n2 -> n2 in
  print_string name;
  (* 3 TOTAL NUMBER OF LETTERS *)
  let lst = List.map (fun x -> Char.escaped x.character) (snd player) in
  let rec helper = function
    | [] -> ""
    | h::t -> h ^ " " ^ helper t in
  let hands = helper lst in
  let sum = List.fold_left (fun a x -> a + x.count) 0 state.letter_bag in
  print_string (concat_space 72 ("letter left: " ^ (string_of_int sum) ^"\n"));
  (* 4 GAME BOARD *)
  print_board state;
  (**ONLY FOR TESTING **)
  (* let () = print_string "\nnew words \n" in
  let () = List.fold_left (fun acc elm -> print_string (elm ^ "\n")) ()
    (collect state.board) in
  let () = print_string "\n" in *)
  (*********************)
  (* 5 COMMANDS *)
  print_message "commands";
  (* 6 SCORE BOARD *)
  print_score state.score_board;
  (* 7 PRINT PLAYER'S HAND *)
  let () = print_string ("\nPlayer's hand: " ^ hands ^ "\n") in ()


(********** REPL **********)

(* [is_tie] : bool
 * true indicating that score_board shows it is a tie game
 * false indicates there is a winner *)
let is_tie lst =
  let scores = List.map (fun x -> snd x ) lst in
  match scores with
  | [] -> false
  | h::t -> List.mem h t

(* [get_players] is a list of players gathered from the user's string input*)
let rec get_players input_string_list =
  match input_string_list with
  |[]   -> []
  |h::[] -> failwith "Invalid Player Input"
  |h1::h2::t -> match String.lowercase_ascii h1 with
            | "human" | "h" -> Human(h2) :: get_players t
            | "ai" | "a"    -> AI(h2)    :: get_players t
            | _      -> failwith "Invalid Player Input"

(* [repl] main repl *)
let rec repl c_state : Data.game_state =
  if 10 = c_state.counter || c_state.quit
  then end_game c_state
  else let pl = fst (current_player_rack c_state) in
  let () = update_gui c_state in
  let new_state = match pl with
    | AI n -> AI.execute_move c_state c_state
    | Human n ->
      let () = print_endline "\nEnter Move" in
      let s_move = read_line() in
      try Human.execute_move s_move c_state with
      | Error_not_center -> print_message "error_not_center"; c_state
      | Error_not_have -> print_message "error_not_have"; c_state
      | Error_not_fit -> print_message "error_not_fit"; c_state
      | Error_not_touching -> print_message "error_not_touching"; c_state
      | Error_not_in_dictionary -> print_message "error_not_in_dictionary"; c_state
      | Error_existing_letter -> print_message "Error_existing_letter"; c_state
      | _ -> let () = print_endline "Invalid command"in c_state in

  let () = print_endline "" in
  repl new_state

and end_game state =
  let help_sort elm1 elm2= Pervasives.compare (snd (elm1)) (snd (elm2)) in
  let winner_list = List.rev (List.sort help_sort state.score_board) in
  let winner = match winner_list with
   | [] -> failwith "No Player"
   | h::t -> (match fst h with Human n1 -> n1 | AI n2 -> n2) in
  let () = if is_tie winner_list then
  ANSITerminal.print_string [ANSITerminal.magenta] ("TIE GAME!\n")
  else ANSITerminal.print_string [ANSITerminal.magenta]
    ("COGRATULATIONS " ^ String.uppercase_ascii winner ^ "!\n\n") in
  print_score winner_list;
  print_string "\n* New Game? --> [play]";
  print_string "\n* Quit Game? --> [quit]\n";
  let rec helper str =
  match String.lowercase_ascii str with
  | "play" -> print_string "Please type [play] or [quit]\n\n";
    initialize_game ()
  | "quit" -> print_string "Thank you for playing!\n\n"; state
  | _ -> print_string "Please type [play] or [quit]\n\n"; helper (read_line ()) in
  helper (read_line())

and initialize_game () =
   ANSITerminal.print_string [ANSITerminal.green]
   "\n\nPlease Enter the Players and Names\n";
   print_endline "\n* Type [Human Eric] or [H Eric] for human player";
   print_endline "* Type [AI Kenta] or [A Kenta] for AI player\n";
   let rec helper str =
      let split = Str.split (Str.regexp " +") (str ^ " ") in
      let player_list = try get_players split with Failure _ -> [] in
        match player_list with
        | [] -> print_string "Invalid players\n\n"; helper (read_line ())
        | _ -> repl (setup player_list) in
    helper (read_line ())

let main_menu () =
  ANSITerminal.print_string [ANSITerminal.magenta]
    ("\n=== WELCOME TO MULTIPLAYER SCRABBLE! ===\n\n");
  print_string "\n* Tutorial? --> [help]";
  print_string "\n* New Game? --> [play]";
  print_string "\n* Quit Game? --> [quit]\n\n";
  let rec helper2 str =
  match String.trim (String.lowercase_ascii str) with
    | "quit" -> print_string "Thank you for playing!\n\n";
    | "play" -> (fun x -> ()) (initialize_game ())
    | "help" -> print_string "message. Please type [play] or [quit]\n\n";
      helper2 (read_line ())
    | _ -> print_string "Please type [play] or [quit]\n\n"; helper2 (read_line ()) in
  helper2 (read_line())
