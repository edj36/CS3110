open Data
open Utils
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
  print_string ("Turn: " ^ (string_of_int state.turn) ^ "\n");
  let player = current_player_rack state in
  let name = match fst player with
    | Human n1 -> n1
    | AI n2 -> n2 in
  print_string (name ^ "\n");
  let lst = List.map (fun x -> Char.escaped x.character) (snd player) in
  let rec helper = function
    | [] -> ""
    | h::t -> h^ " " ^ helper t in
  let hands = helper lst in
  let sum = List.fold_left (fun a x -> a + x.count) 0 state.letter_bag in
  let () = print_string ("letter left: " ^ (string_of_int sum) ^"\n") in
  let () = print_board state in
  let () = print_string "\nold words \n" in
  let () = List.fold_left (fun acc elm -> print_string (elm ^ "\n")) () state.words in
  let () = print_string "\nnew words \n" in
  let () = List.fold_left (fun acc elm -> print_string (elm ^ "\n")) ()
    (get_newwords (collect state.board) state.words) in
  let () = print_string "\n" in
  let () = print_score state.score_board in
  let () = print_string "\n" in
  let () = print_string ("Player's hand: " ^ hands ^ "\n")
  in ()
