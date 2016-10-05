open OUnit2
open Engine




module ListEngineTester = ListEngine 
module TreeEngineTester = TreeEngine
  
let tests = [
  "empty" >:: (fun _ -> assert_equal [] 
  	(ListEngineTester.index_of_dir "" |> ListEngineTester.to_list));
]


(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)