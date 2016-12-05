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

(* [is_not_srounded] represents bool type, indicating if the input coordicate
 * is not sourounded by letters
 * true -> the coordinate is not sourounded by letters
 * false -> there is at least 1 letter adjacent to the coordinate *)
let is_not_srounded (x,y) board =
  let check (x,y) =
    if x = 15 || x = -1 then true
    else if y = 15 || y = -1 then true
    else let tile = get_tile (x, y) board in
    match tile.letter with
    | None -> true | Some _ -> false in
  (check (x+1, y)) && (check (x-1, y)) && (check (x, y+1)) && (check (x, y-1))

(* [is_fit] is bool represetationg after checking if the word will fit on the
 * board given starting coordinate and string and direction*)
let is_fit (x,y) str dir =
  let n = String.length str in
  match dir with | Across -> y+n <= 15 | Down -> x+n <= 15

(* [iterate_word_lst] iterates over [lst] and checks if each word
 * is in the scrabble dictionary, true if all in the dictionary,
 * false otherwise *)
let rec iterate_word_lst lst = match lst with
  | [] -> true
  | h :: t -> (find_word (String.uppercase_ascii h)) && (iterate_word_lst t)

(* [is_valid] is a boolean indicating the validity of [move] in [state]
 * [shuffle] -> true
 * [swapall] -> true
 * [end] -> true
 * [pass] -> true
 * [swapsome] -> true if all the letters swapping are in the hand
 * [play] -> true if it's valid (see SPEC for [validate]) *)
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
    if old_board = new_board then false
    else
    (* step 0 : DOES IT FIT ON THE BOARD *)
    if not (is_fit (translate_coodinate crd) str dir) then raise Error_not_fit
    else
    (* step 1 : IS CENTER COVERED *)
    let tile = get_tile (7,7) new_board in
    let case1 = (match (tile.letter) with
    | None -> false
    | Some _ -> true) in if not case1 then raise Error_not_center
    else
    (* step 2 : ARE NEWLY FORMED WORDS ALL VALID *)
    let new_words = collect new_board in
    if not (iterate_word_lst new_words) then raise Error_not_in_dictionary
    else
    (* step 3 : ARE NEW WORDS MADE ONLY BY THE LETTERS IN HAND *)
    let new_crds =
      get_newcoordinates (collect_coordinates new_board) (collect_coordinates old_board) in
    let new_letters = get_newletters new_crds new_board in
    if not (check_char new_letters (current_player_rack state))
    then raise Error_not_have
    else
    (*step 4 : IS THE NEW WORD ADJACENT TO THE EXISITING LETTER ON THE BOARD*)
    let case4 =
    not (List.fold_left (fun a e -> a && is_not_srounded e old_board) true new_crds) in
    (match collect old_board with
    | [] -> true
    | _ -> if not case4 then raise Error_not_touching else true)
  | SwitchAll -> true
  | SwitchSome c_list -> check_char (List.map (fun x -> String.uppercase_ascii x) c_list)
   (current_player_rack state)
  | Pass -> true
  | Shuffle -> true
  | End -> true

(* [validate] is a bool representation indicating if the move is valid or not
 * check following criteria
 * - the word is made only by the letters in the hands (Done)
 * - check if word fits in the board
 * - the new words is a valid word in dictionary
 * - when it's turn 1, you have to place over (H,8)
 * - new words have to satisfy one of following rules
 *   1, Adding one or more letters to a word or letters already on the board
 *     (only case that you can play 1 letter)
 *   2, Placing a word at right angles to a word already on the board
 *   3, Placing a complete word parallel to a word already played
      so that adjacent letters also form complete words *)
let validate move state =
  if is_valid move state then update move state else state
