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

let char_to_int_brd c = match c with
  | 'a' -> 0 | 'b' -> 1 | 'c' -> 2 | 'd' -> 3 | 'e' -> 4 | 'f' -> 5 | 'g' -> 6
  | 'h' -> 7 | 'i' -> 8 | 'j' -> 9 | 'k' -> 10 | 'l' -> 11 | 'm' -> 12
  | 'n' -> 13 | 'o' -> 14 | _ -> failwith "out of bounds"

let get_xy crd = match crd with
  | (y,x) -> (char_to_int_brd y, (x-1))

(* [make_possible_str] is the list of letters/spaces that represents the
 * column/row of characters on the tiles on [board] starting at [coord] and
 * going in direction [dir], a space " " represents an empty tile *)
(* TODO: need to fix get_nextcoordinate *)
let rec make_possible_str dir coord board =
  let x = snd coord in
  let y = fst coord in
  match dir with
  | Across -> if y > 14 then [] else let ty = (get_tile coord board) in
    let stry = begin match ty.letter with
      | None -> " "
      | Some m -> Char.escaped m
    end in
    let next_cy = begin try get_nextcoordinate (x,y) dir with
      | Failure _ -> (15,15)
    end in stry :: (make_possible_str dir next_cy board)
  | Down -> if x > 14 then [] else let tx = (get_tile coord board) in
    let stry = begin match tx.letter with
      | None -> " "
      | Some n -> Char.escaped n
    end in
    let next_cx = begin try get_nextcoordinate (x,y) dir with
      | Failure _ -> (15,15)
    end in stry :: (make_possible_str dir next_cx board)

(* [rack_contains] is a boolean indicating if [rack] contains [e] *)
let rack_contains rack e =
  let c_rack = List.map (fun x -> (Char.uppercase_ascii (x.character))) rack in
  List.mem (Char.uppercase_ascii e) c_rack

(* [rack_remove] is a new rack made from removing [e] from [rack] *)
let rec rack_remove rack e = match rack with
  | [] -> []
  | h :: t -> if (Char.uppercase_ascii h.character) = (Char.uppercase_ascii e)
    then t
    else h :: (rack_remove t e)

let rec print_lst lst = match lst with
  | [] -> ()
  | h::t -> let _ = print_endline h in print_lst t

(* [iterate_brd_lst] goes over each tile in [brd_lst] checking to
 * see if the tile is equal to its corresponding letter in [str] (at the
 * same position) or inserts the letter in [str] at the position into the
 * spot in a brd_lst (if it is in [player]'s rack) true is each letter
 * fits and the letters that need to be are in the player's rack,
 * false otherwise, once its at the end of the string, if the next tile is
 * empty, return true, else check if the word created is valid, if it is,
 * continue checking tiles, else, return false *)
let rec iterate_brd_lst str rack n brd_lst = match brd_lst with
  | [] -> true
  | h :: t when (n >= (String.length str)) -> (* add h to end of str, check if its in srabble dict *)
   (* check if h is empty string*)
    if h = " " then true else
    let n_str = String.trim (String.uppercase_ascii (str ^ h)) in
      if not (find_word n_str) then
      let _ = print_endline "error line 80" in false
      (* if in scrabble dict, move on, increment n *)
      else iterate_brd_lst n_str rack (n+1) t
  | h :: t when h = " " -> (* check if nth position is in rack *)
    let nth_char = Char.uppercase_ascii str.[n] in
    (* if not in rack, return false*)
    if not (rack_contains rack nth_char) then
    let _ = print_endline "error line 87" in false
    (* if in rack, remove that element from rack, move on, increment n *)
    else let new_rack = rack_remove rack nth_char in
      iterate_brd_lst str new_rack (n+1) t
  | h :: t -> (* check h vs nth position in str *)
    let nth = String.uppercase_ascii (Char.escaped str.[n]) in
      (* if h <> nth letter (and h <> " "), return false *)
      if (String.uppercase_ascii h) <> (nth) then
      let _ = print_endline "error line 95" in false
      (* if h = nth letter, move on, increment n *)
      else iterate_brd_lst str rack (n+1) t




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
    (* step 1 *)
    let up_str = String.uppercase_ascii str in
    if not (find_word up_str) then
    let _ = print_endline "error line 117" in false
    (* step 2 *)
    else
    let xy = get_xy crd in
    let brd_lst = make_possible_str dir xy state.board in
    let rack = snd (current_player_rack state) in
    (* step 3 and 4 *)
    if not (iterate_brd_lst up_str rack 0 brd_lst) then
    let _ = print_endline "error line 128" in false
    (* step 5 *)
    else let word_lst = collect state.board in
    if not (iterate_word_lst word_lst) then
    let _ = print_endline "error line 129" in false
    (* step 6 *)
    else let temp_state = update move state in
    let center = (get_tile (7,7) temp_state.board) in
    begin match center.letter with
      | None -> let _ = print_endline "error line 134" in false
      | Some _ -> true
    end
  | SwitchAll -> true
  | SwitchSome c_list -> check_char (List.map (fun x -> Char.uppercase_ascii x) c_list)
   (current_player_rack state)
  | Pass -> true
  | Shuffle -> true
  | _ -> let _ = print_endline "error line 141" in false

(* Algorithm:
  1. check if word is in scrabble dictionary
  2. go to start coordinates and face in the right direction, make string
     going till end of board
  3. for length of the string entered:
    - check tile @ position in string you just made, if occupied, make sure
      it equals the corresponding letter in string entered
    - if not occupied, fill it with next character in string entered
      (first check that that character is available in the player's rack)
  4. once at end of string, check next tile
    - if empty, return true
    - if there is a letter there, add the letter to the word, then check if this
      new word is in the scrabble dictionary
    - if the word is not in scrabble dictionary return false, if it is,
      repeat step 4.
  5. derefence and make a copy of state, then call kenta's helper function with the
    copy of state and string entered, this will return a list of the words it creates
    on the board. Loop through this list, check if each word is in scrabble dictionary
    if any is not, return false, else if you get to end of list return true.
  6. check that center is covered
*)


(*let rec list_tl lst = match lst with
| [] -> failwith "this is bad"
| h :: [] -> h
| h :: t -> list_tl t *)

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
