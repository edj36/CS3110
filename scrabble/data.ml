(* data structures for the game *)

module Data = struct 

  type letter = { character: char; pt : int; mutable count : int }

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

  type tile =
    {
      bonus : bonus_status;
      letter: char option;
    }

  type player =
    | Human of string
    | AI of string

  type scrabble_board = tile array array

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
    words : string list
  }

end

(*type scrabble_dictionary = string two_three_tree*)

