open Data
open Utils
open Player
open State
open Yojson.Basic.Util

(*********** GUI ***********)
let y_axis num =
  if (num+1) > 9 then string_of_int (num+1) else " " ^ (string_of_int (num+1))

(* [print_board] print string *)
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

let rec concat_space num str =
  if num = 0 then str else concat_space (num-1) (" " ^ str)

let get_namelength rack =
  let name = match fst rack with Human n1 -> n1 | AI n2 -> n2 in
  String.length name

(* [print_score] score board *)
let rec print_score = function
  | []-> ()
  | (x,y)::t -> let name = (match x with
    |Human n1 -> n1
    |AI n2 -> n2) in
  print_string (name ^ ": " ^ (string_of_int y) ^ " \n");
  print_score t

(* [hpdate_gui] *)
let update_gui state =
  print_string ("\n\nTurn: " ^ (string_of_int (state.turn + 1)) ^ "\n");
  let player = current_player_rack state in
  let name = match fst player with
    | Human n1 -> n1
    | AI n2 -> n2 in
  print_string name;
  let lst = List.map (fun x -> Char.escaped x.character) (snd player) in
  let rec helper = function
    | [] -> ""
    | h::t -> h ^ " " ^ helper t in
  let hands = helper lst in
  let sum = List.fold_left (fun a x -> a + x.count) 0 state.letter_bag in
  print_string (concat_space 72 ("letter left: " ^ (string_of_int sum) ^"\n"));
  print_board state;
  (**ONLY FOR TESTING **)
  let () = print_string "\nold words \n" in
  let () = List.fold_left (fun acc elm -> print_string (elm ^ "\n")) () state.words in
  let () = print_string "\nnew words \n" in
  let () = List.fold_left (fun acc elm -> print_string (elm ^ "\n")) ()
    (get_newwords (collect state.board) state.words) in
  let () = print_string "\n" in
  (*********************)
  print_score state.score_board;
  let () = print_string ("\nPlayer's hand: " ^ hands ^ "\n")
  in ()


(********** REPL **********)

(* [is_tie] is a bool indicating is givien score_board results tie *)
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
  if List.length (c_state.score_board) = c_state.counter
  then end_game c_state
  else let pl = fst (current_player_rack c_state) in
  let () = update_gui c_state in
  let new_state = match pl with
    | AI n -> AI.execute_move c_state c_state
    | Human n ->
      let () = print_endline "\nEnter Move" in
      let s_move = read_line() in
      try Human.execute_move s_move c_state with
      |Failure _ -> let () = print_endline "Invalid command" in c_state in
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
