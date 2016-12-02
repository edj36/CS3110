open Data
open Utils
open State
open Tree

(* [check_char] represents bool type, indicating if all elements in char List
 * is a member of [hands]. Also accounts for duplicates
 * ex) if you have 2 'A's, ['A';'A'] -> true but ['A';'A';'A'] -> false *)
let check_char lst player =
  let hands = letter_to_char (snd player) in
  let rec helper lst hands = match lst with
    | []-> true
    | h::t -> List.mem h hands && helper t (remove h hands) in
      helper lst hands

let is_not_srounded (x,y) board =
  let check (x,y) =
    if x = 15 || x = -1 then true
    else if y = 15 || y = -1 then true
    else let tile = get_tile (x, y) board in
    match tile.letter with
    | None -> true | Some _ -> false in
  (check (x+1, y)) && (check (x-1, y)) && (check (x, y+1)) && (check (x, y-1))

let is_fit (x,y) str dir =
  let n = String.length str in
  match dir with
  | Across -> y+n <= 15
  | Down -> x+n <= 15

(* [iterate_word_lst] iterates over [lst] and checks if each word
 * is in the scrabble dictionary, true if all in the dictionary,
 * false otherwise *)
let rec iterate_word_lst lst = match lst with
  | [] -> true
  | h :: t -> (find_word (String.uppercase_ascii h)) && (iterate_word_lst t)

(* [is_valid] is a boolean indicating the validity of [move] in [state] *)
let is_valid move state = match move with
  | Play
    {
      word = str;
      direction = dir;
      coordinate = crd
    } ->
    let old_board = state.board in
    let new_board = try place_string str dir (translate_coodinate crd) old_board with
    | Failure _ -> old_board in

    if old_board = new_board then
      let _ = print_endline "Cannot override exsiting letter" in false
    else

    (* step 0 : DOES IT FIT ON THE BOARD *)
    if not (is_fit (translate_coodinate crd) str dir) then
      let _ = print_endline "word doesnt fit!" in false
    else

    (* step 1 : IS CENTER COVERED ? *)
    let tile = get_tile (7,7) new_board in
    let case1 = (match (tile.letter) with
    | None -> false
    | Some _ -> true) in if not case1
    then let _ = print_endline "you have to play across center!" in false
    else

    (* step 2 : ARE NEWLY FORMED WORDS ALL VALID ?*)
    let new_words = get_newwords (collect new_board) (collect old_board) in
    if not (iterate_word_lst new_words)
    then let _ = print_endline "not a valid word!" in false
    else

    (* step 3 : make sure you didnt play letters that are not in hands *)
    let new_crds =
      get_newcoordinates (collect_coordinates new_board) (collect_coordinates old_board) in
    let new_letters = get_newletters new_crds new_board in
    if not (check_char new_letters (current_player_rack state))
    then let _ = print_endline "you can't use a letter you don't have!" in false
    else

    (*step 4 : make sure at least one letter is played next to existing letter *)
    let case4 =
    not (List.fold_left (fun a e -> a && is_not_srounded e old_board) true new_crds) in
    (match collect old_board with
    | [] -> true
    | _ ->
    if not case4 then
      let _ = print_endline "new word has to be adjacent to the existing word!" in
      false
    else true)


  | SwitchAll -> true
  | SwitchSome c_list -> check_char (List.map (fun x -> Char.uppercase_ascii x) c_list)
   (current_player_rack state)
  | Pass -> true
  | Shuffle -> true
  | _ -> let _ = print_endline "error line 141" in false

(* [validate] is a bool representation indicating if the move is valid or not
 * check following criteria
 * - the word is made only by the letters in the hands (Done)
 * - the check if word fits in the board
 * - TODO the new words is a valid word in dictionary
 * - TODO when it's turn 1, you have to place over (H,8)
 * - TODO new words have to satisfy one of following rules
 *   1, Adding one or more letters to a word or letters already on the board
 *     (only case that you can play 1 letter)
 *   2, Placing a word at right angles to a word already on the board
 *   3, Placing a complete word parallel to a word already played
      so that adjacent letters also form complete words *)

let validate move state =
  if is_valid move state then update move state else state
