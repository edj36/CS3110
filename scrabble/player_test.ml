open OUnit2
open Data
open Utils
open Player
open State

let players = [Human "A"; Human "B"]
let state = setup players
let ahand = List.map (fun x -> char_to_letter x state.letter_bag )
  ['A';'B';'C';'D';'E';'F';'G']
let bhand = List.map (fun x -> char_to_letter x state.letter_bag )
  ['H';'I';'J';'K';'L';'M';'N']
let test_state =
{
  board = state.board;
  score_board = state.score_board;
  letter_bag = state.letter_bag;
  player_racks = [(Human "A",ahand);(Human "B", bhand)];
  turn = state.turn;
  words = state.words;
  counter = 0
}
let update1 = Human.execute_move "play abc a a 1" test_state
let initialize = [
  "Initialize 1"  >:: (fun _ -> assert_equal 0  test_state.turn);
  "Initialize 2"  >:: (fun _ -> assert_equal []  test_state.words);
]
let score = [
  "score 1" >:: (fun _ -> assert_equal 0 (get_score test_state "A"));
]
let tests = initialize @ score
