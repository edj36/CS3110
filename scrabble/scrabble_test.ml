open OUnit2
open Data
open Utils
open Player
open Filter
open Move
open Server
open State 


let suite = "scrabble test suite" >:::
  Move_test.tests

let _ = run_test_tt_main suite