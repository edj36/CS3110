module Data = struct

  type letter = { character: char; pt : int; mutable count : int }

  type coordinate = char * int

  type direction =
    | Across
    | Down

  type bonus_status =
    | Double_letter
    | Double_word
    | Triple_letter
    | Triple_word
    | Center
    | Normal

  type moves =
    | Play of string * direction * coordinate
    | Draw
    | SwitchAll
    | SwitchSome of letter list
    | Pass
    | Shuffle

  type tile =
    {
      bonus : bonus_status;
      letter: letter option;
    }

  type player =
    | Human of string
    | AI of string

  type scrabble_board = tile array array
  type score_board = (player * int) list
  type letter_bag = letter list
  type player_rack = (player * letter option list)

  type game_state =
  {
    board : scrabble_board;
    score_board : score_board;
    letter_bag : letter_bag;
    player_racks : player_rack list;
    turn : int
  }

(*type scrabble_dictionary = string two_three_tree*)

end
