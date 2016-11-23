open OUnit2
open Data
open Player


let get_move = [
  "Test 1"  >:: (fun _ -> assert_equal 0  0 );

]


let tests =
  "tests" >:::
  ["test suite for get_move"  >::: get_move
   ]

let _ = run_test_tt_main tests