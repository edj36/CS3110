open OUnit2

let suite = "scrabble test suite" >:::
  Move_test.tests

let _ = run_test_tt_main suite