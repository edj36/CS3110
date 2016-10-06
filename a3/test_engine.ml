open OUnit2
open Engine




module ListEngineTester = ListEngine
module TreeEngineTester = TreeEngine

let list_test2 = ListEngineTester.index_of_dir "test2"
let tree_test2 = TreeEngineTester.index_of_dir "test2"

let simple_list = [("people", ["test0/small.txt"]);
					("the", ["test0/small.txt"]);
					("we", ["test0/small.txt"])]

let tests = [

	(*Exceptions*)
  	(*"empty" >:: (fun _ -> assert_raises (Engine.Not_found)
  		(fun _ -> (ListEngineTester.index_of_dir "")));*)

	(*index_of_dir and to_list*)

	(*to list of tree and list*)
	(* THIS IS TEH FAILING TEST
	 -the lists should be the same, but its throwing errors *)
	"test to_list" >:: (fun _ -> assert_equal
			(ListEngineTester.to_list list_test2)
            (TreeEngineTester.to_list tree_test2));

  	"test0 list" >:: (fun _ -> assert_equal simple_list
  		((TreeEngineTester.index_of_dir "test0") |> TreeEngineTester.to_list));
	"test0 tree" >:: (fun _ -> assert_equal simple_list
  		((TreeEngineTester.index_of_dir "test0") |> TreeEngineTester.to_list));

	(*AND NOT*)

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