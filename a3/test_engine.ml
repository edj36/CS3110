open OUnit2
open Engine




module ListEngineTester = ListEngine
module TreeEngineTester = TreeEngine

let list_idx = ListEngineTester.index_of_dir "test1"
let tree_idx = TreeEngineTester.index_of_dir "test1"

let simple_list = [("people", ["test0/small.txt"]);
					("the", ["test0/small.txt"]);
					("we", ["test0/small.txt"])]

let tests = [

	(*Exceptions*)
  	(*"empty" >:: (fun _ -> assert_raises Engine.Not_found
  		(fun _ -> (ListEngineTester.index_of_dir ""))); *)

	(*index_of_dir and to_list*)

	(*to list of tree and list*)
	"test1 to_list" >:: (fun _ -> assert_equal
			(ListEngineTester.to_list list_idx)
            (TreeEngineTester.to_list tree_idx));

  	"test0 list" >:: (fun _ -> assert_equal simple_list
  		((TreeEngineTester.index_of_dir "test0") |> TreeEngineTester.to_list));
	"test0 tree" >:: (fun _ -> assert_equal simple_list
  		((TreeEngineTester.index_of_dir "test0") |> TreeEngineTester.to_list));

	(*AND NOT*)
	"test and_not list" >:: (fun _ -> assert_equal 
		[]
  		(ListEngineTester.and_not list_idx ["we";"the"] [""]));

	"test and_not tree"

	(*AND*)

	(*OR NOT*)

	(*OR*)

	(*
	 * single quotes
	 random escaped characters
	 http://gmail.com is a word
	 *
	 *)

]


(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)