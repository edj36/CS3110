type letter = { character: char; pt : int }

type bonus_status =
  | Double_letter
  | Double_word
  | Triple_letter
  | Triple_word
  | Center
  | Normal

type move = 
  | Move of (letter * int * int) list 
  | Draw  

type tile =
{
  coordinates : (int * int);
  bonus : bonus_status;
  isOccupied : bool;
  char: char option
}

type player = 
  | Human of string 
  | AI of string 

type scrabble_board = tile list list
type score_board = (player * int) list
type letter_bag = letter list
type player_rack = letter list

type game_state =
{
  board : scrabble_board;
  score_board : score_board;
  letter_bag : letter_bag;
  player_racks : player_rack list;
  turn : int
}

type scrabble_dictionary = string two_three_tree
