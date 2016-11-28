open Data
open Utils

type state = game_state
type m = move

(* [check_char] represents bool type, indicating if all elements in char List
 * is a member of [hands]. Also accounts for duplicates
 * ex) if you have 2 'A's, ['A';'A'] -> true but ['A';'A';'A'] -> false *)
let check_char lst (rack : player_rack) =
  let hands = letter_to_char (snd rack) in
  let rec helper lst hands =
  match lst with
  | []-> true
  | h::t -> List.mem h hands && helper t (remove h hands) in
  helper lst hands

(*let search str dict = failwith "Unimplemented"*)

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
let validate move (state:state) : bool=
  match move with
  | Play
    {
      word = str;
      direction = dir;
      coordinate = crd
    } ->
    let player = current_player state in
    let chr_list = string_to_char_list str in
    check_char chr_list player 
  | _ -> true
