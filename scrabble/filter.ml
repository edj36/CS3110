open Data
open API

type state = game_state
type move = moves

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
