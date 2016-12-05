open OUnit2
open Filter
open Data
open Utils
open Player
open State

let players = [Human "A"]
let state = setup players
let a_hand = List.map (fun x -> char_to_letter x state.letter_bag )
  ["A";"B";"C";"D";"E";"F";"G"]

let test_state =
{
  board = state.board;
  score_board = state.score_board;
  letter_bag = state.letter_bag;
  player_racks = [(Human "A", a_hand)];
  turn = state.turn;
  counter = 0;
  quit = false
}


let tests = []
