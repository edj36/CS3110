open OUnit2
open Engine




module ListEngineTester = ListEngine
module TreeEngineTester = TreeEngine

let list_idx = ListEngine.index_of_dir "test1"
let tree_idx = TreeEngine.index_of_dir "test1"

let simple_list = [("people", ["test0/small.txt"]);
					("the", ["test0/small.txt"]);
					("we", ["test0/small.txt"])]

let tests = 
[
	"empty index_of_dir list " >:: (fun _ -> assert_equal []
  		((ListEngine.index_of_dir "test_empty") |> ListEngine.to_list));
	"empty index_of_dir tree " >:: (fun _ -> assert_equal []
  		((TreeEngine.index_of_dir "test_empty") |> TreeEngine.to_list));
	"test1 to_list" >:: (fun _ -> assert_equal
			(ListEngine.to_list list_idx)
            (TreeEngine.to_list tree_idx));
  	"test0 index_of_dir list" >:: (fun _ -> assert_equal simple_list
  		((ListEngine.index_of_dir "test0") |> ListEngine.to_list));
	"test0 index_of_dir tree" >:: (fun _ -> assert_equal simple_list
  		((TreeEngine.index_of_dir "test0") |> TreeEngine.to_list));
	"test 1 and_not list" >:: (fun _ -> assert_equal 
		["test1/medium.txt";"test1/small.txt"]
  		(ListEngine.and_not list_idx ["we";"the"] ["apple"]));
	"test 1 and_not tree" >:: (fun _ -> assert_equal 
		["test1/medium.txt";"test1/small.txt"]
  		(TreeEngine.and_not tree_idx ["we";"the"] ["apple"]));
	"test 2 and_not list" >:: (fun _ -> assert_equal 
		["test1/small.txt"]
  		(ListEngine.and_not list_idx ["we";"the"] ["ourselves"]));
	"test 2 and_not tree" >:: (fun _ -> assert_equal 
		["test1/small.txt"]
  		(TreeEngine.and_not tree_idx ["we";"the"] ["ourselves"]));
	"test 1 and list" >:: (fun _ -> assert_equal 
		["test1/medium.txt";"test1/small.txt"]
  		(ListEngine.and_not list_idx ["we";"the"] []));
	"test 1 and tree" >:: (fun _ -> assert_equal 
		["test1/medium.txt";"test1/small.txt"]
  		(TreeEngine.and_not tree_idx ["we";"the"] []));
	"test 2 and list" >:: (fun _ -> assert_equal 
		["test1/medium.txt"]
  		(ListEngine.and_not list_idx ["we";"common"] []));
	"test 2 and tree" >:: (fun _ -> assert_equal 
		["test1/medium.txt"]
  		(TreeEngine.and_not tree_idx ["we";"common"] []));
	"test 3 and list" >:: (fun _ -> assert_equal 
		[] (ListEngine.and_not list_idx ["apple"] []));
	"test 3 and tree" >:: (fun _ -> assert_equal 
		[] (TreeEngine.and_not tree_idx ["apple"] []));
	"test 1 or_not list" >:: (fun _ -> assert_equal 
		["test1/medium.txt";"test1/small.txt"]
  		(ListEngine.or_not list_idx ["we";"the"] ["apple"]));
	"test 1 or_not tree" >:: (fun _ -> assert_equal 
		["test1/medium.txt";"test1/small.txt"]
  		(TreeEngine.or_not tree_idx ["we";"the"] ["apple"]));
	"test 2 or_not list" >:: (fun _ -> assert_equal 
		["test1/small.txt"]
  		(ListEngine.or_not list_idx ["we";"the"] ["ourselves"]));
	"test 2 or_not tree" >:: (fun _ -> assert_equal 
		["test1/small.txt"]
  		(TreeEngine.or_not tree_idx ["we";"the"] ["ourselves"]));
	"test 1 or list" >:: (fun _ -> assert_equal 
		["test1/medium.txt";"test1/small.txt"]
  		(ListEngine.or_not list_idx ["we";"the"] []));
	"test 1 or tree" >:: (fun _ -> assert_equal 
		["test1/medium.txt";"test1/small.txt"]
  		(TreeEngine.or_not tree_idx ["we";"the"] []));
	"test 2 or list" >:: (fun _ -> assert_equal 
		["test1/medium.txt";"test1/small.txt"]
  		(ListEngine.or_not list_idx ["we";"common"] []));
	"test 2 or tree" >:: (fun _ -> assert_equal 
		["test1/medium.txt";"test1/small.txt"]
  		(TreeEngine.or_not tree_idx ["we";"common"] []));
	"test 3 or list" >:: (fun _ -> assert_equal 
		[] (ListEngine.or_not list_idx ["apple";"banana"] []));
	"test 3 or tree" >:: (fun _ -> assert_equal 
		[] (TreeEngine.or_not tree_idx ["apple";"banana"] []));
]


(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)