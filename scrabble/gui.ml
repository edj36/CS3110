open Data
open Utils
open State
open Yojson.Basic.Util

(*********** GUI ***********)

(* [x_axis] is a string type representation of y_axis label of the scrabble
 * board *)
let x_axis num =
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
  print_string [white] (x_axis j);
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
      | Some c -> ([on_yellow; black], " " ^ c ^ "  ") in
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
    | AI (n2,i) -> n2 in print_string name;
  (* 3 TOTAL NUMBER OF LETTERS *)
  let sum = List.fold_left (fun a x -> a + x.count) 0 state.letter_bag in
  print_string (concat_space 72 ("letter left: " ^ (string_of_int sum) ^"\n"));
  (* 4 GAME BOARD *)
  print_board state;
  (* 5 COMMANDS *)
  print_message "commands";
  (* 6 SCORE BOARD *)
  print_score state.score_board;
  (* 7 PRINT PLAYER'S HAND *)
  let rec color_print_hand lst f =
    let open ANSITerminal in
    match lst with
    | [] -> print_string [on_black] "\n"
    | h::t -> print_string [on_yellow; black] (f h);
    print_string [on_black] " "; color_print_hand t f in
  let () = print_string ("\nPlayer's hand  ") in
  let () = color_print_hand (snd player)
    (fun x -> " " ^ x.character ^ " ") in
  let () = print_string ("Letter points  ") in
  let () = color_print_hand (snd player)
    (fun x -> if x.pt > 9 then " " ^ string_of_int x.pt
      else " " ^ string_of_int x.pt ^ " ") in ()
