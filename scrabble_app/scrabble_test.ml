open OUnit2
open Data
open Utils
open Player
open Filter
open Server
open State


let suite = "scrabble test suite" >:::
  Player_test.tests
  @ Filter_test.tests
  @ State_test.tests
  @ Tree_test.tests

let _ = run_test_tt_main suite
