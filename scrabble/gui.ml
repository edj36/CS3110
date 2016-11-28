open Data
open Utils


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
