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

(* [make_possible_string] is the list of letters/spaces that represents the 
 * column/row of characters on the tiles on [board] starting at [coord] and 
 * going in direction [dir], a space " " represents an empty tile *)
let make_possible_string dir coord board = failwith "Unimplemented"

(* [iterate_brd_lst] goes over each tile in [brd_lst] checking to 
 * see if the tile is equal to its corresponding letter in [str] (at the 
 * same position) or inserts the letter in [str] at the position into the 
 * spot in a brd_lst (if it is in [player]'s rack) true is each letter 
 * fits and the letters that need to be are in the player's rack, 
 * false otherwise, once its at the end of the string, if the next tile is 
 * empty, return true, else check if the word created is valid, if it is, 
 * continue checking tiles, else, return false *)
let iterate_brd_lst str player brd_lst = match brd_lst with 
| [] -> true 
| h :: t -> failwith "Unimplemented"

(* [dereference] is a new state object that is a copy of [state] but 
 * because [state] is mutable, everything dereferenced so that updating 
 * the new state doesn't ruin anything *)
let dereference state = failwith "Unimplemented"

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
    if not (find_word up_str) then false 
    (* step 2 *)
    else let brd_lst = make_possible_string dir crd state.board in 
    let player = fst (current_player_rack state) in 
    (* step 3 and 4 *)
    if not (iterate_brd_lst up_str player brd_lst) then false 
    (* step 5 *)
    else let new_state = dereference state in 
    let word_lst = collect new_state.board in 
    if not (iterate_word_lst word_lst) then false 
    (* step 6 *)
    else let center = get_tile (7,7) new_state in 
    begin match center.letter with 
      | None -> false 
      | Some _ -> true
    end 
  | SwitchAll -> true 
  | SwitchSome c_list -> check_char c_list (current_player_rack state)
  | Pass -> true
  | Shuffle -> true
  | _ -> false 

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
