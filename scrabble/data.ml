(* data structures for the game *)

(* EXCPETIONS *)
exception Error_existing_letter
exception Error_not_fit
exception Error_not_center
exception Error_not_in_dictionary
exception Error_not_have
exception Error_not_touching
exception Error_too_many_players
exception Error_duplicate_names
exception Error_ai_level
exception Invalid

type letter = { character : char; pt : int; mutable count : int }

type coordinate = char * int

type direction = | Across | Down

type bonus_status =
  | Double_letter
  | Double_word
  | Triple_letter
  | Triple_word
  | Center
  | Normal

type move =
  | Play of
    {
      word : string;
      direction : direction;
      coordinate : coordinate
    }
  | SwitchAll
  | SwitchSome of char list
  | Pass
  | Shuffle
  | End

type tile =
  {
    bonus : bonus_status;
    letter: char option;
  }

type player =
  | Human of string
  | AI of (string * int)

type scrabble_board = tile list list

type score_board = (player * int) list

type letter_bag = letter list

type player_rack = (player * letter list)

type game_state =
{
  board : scrabble_board;
  score_board : score_board;
  letter_bag : letter_bag;
  player_racks : player_rack list;
  turn : int;
  counter : int;
  quit : bool
}



(*type scrabble_dictionary = string two_three_tree*)
