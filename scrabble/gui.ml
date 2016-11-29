open Data
open Utils

(*********** GUI ***********)

let print_board state =
  ANSITerminal.(print_string [] "\n";
  let b = state.board in
  for j = 0 to 14 do
    for i = 0 to 14 do
      let tile = get_tile (j,i) b in
      let (lst, str) = match tile.letter with
      | None -> ( match tile.bonus with 
        | Double_letter -> ([on_cyan; black], " DL ")
        | Double_word -> ([on_blue; black], " DW ")
        | Triple_letter -> ([on_blue; white], " TL ")
        | Triple_word -> ([on_red; black], " TW ")
        | Center -> ([on_magenta; white], " CE ")
        | Normal -> ([on_white; black], " ** " ) )
      | Some c -> ([on_yellow; black], " " ^ Char.escaped c ^ "  ") in
      print_string [] " "; print_string lst str; print_string [] " ";
    done;
    print_string [Reset] "\n \n";
  done )


let update_gui state =
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
