open OUnit2
open Data

module type Tests = sig
  val tests : OUnit2.test list
end

module StringComparable = struct
  type t = string
  let compare x y = 
    if Pervasives.compare x y < 0 then `LT 
    else if Pervasives.compare x y = 0 then `EQ 
    else `GT
  let format something somethingelse = () 
end 

module IntComparable = struct
  type t = int
  let compare x y = 
    if Pervasives.compare x y < 0 then `LT 
    else if Pervasives.compare x y = 0 then `EQ 
    else `GT
  let format something somethingelse = () 
end 

(* [DictTester] is where you will implement your test harness
 * to find buggy implementations. *)
module DictTester (M:DictionaryMaker) = struct
  module Dict = M(IntComparable)
  let tests = [
  "name" >:: (fun _ -> assert_equal true (Dict.member 3 (Dict.insert 3 4 Dict.empty)));
  ]
end

module ListDictTester = DictTester(MakeListDictionary)
module TreeDictTester = DictTester(MakeTreeDictionary)

(* [tests] is where you should provide OUnit test cases for
 * your own implementations of dictionaries and sets.  You're
 * free to use [DictTester] as part of that if you choose. *)
let tests = ListDictTester.tests 
			@ TreeDictTester.tests

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)