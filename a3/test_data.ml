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
  module Dict1 = M(StringComparable)

  let dict = Dict.empty |>
    Dict.insert 3 4 |>
    Dict.insert 6 4 |>
    Dict.insert 29 4 |>
    Dict.insert 15 4 |>
    Dict.insert 2 4 |>
    Dict.insert 4 4 |>
    Dict.insert 36 4

  let dictv1 =
    Dict.empty |>
    Dict.insert 3 4 |>
    Dict.insert 6 4 |>
    Dict.insert 29 4 |>
    Dict.insert 15 4 |>
    Dict.insert 2 4 |>
    Dict.insert 4 4

  let divtv = Dict.rep_ok dictv1
  let dictv2 = Dict.remove 3 dictv1

  let dict1 = Dict1.empty |>
    Dict1.insert "a" 4 |>
    Dict1.insert "d" 4 |>
    Dict1.insert "f" 4 |>
    Dict1.insert "b" 4 |>
    Dict1.insert "c" 4 |>
    Dict1.insert "g" 4 |>
    Dict1.insert "e" 4


  let tests = 
  [
  "member" >:: (fun _ -> assert_equal true (Dict.member 3
                          (Dict.insert 3 4 Dict.empty)));
  "find" >:: (fun _ -> assert_equal (Some 5) (Dict.find 3
                          (Dict.insert 3 5 (Dict.insert 3 4 Dict.empty))));
  "to_list" >:: (fun _ -> assert_equal [2;3;4;6;15;29;36]
                          (List.map fst (Dict.to_list dict)));
  "removetest" >:: (fun _ -> assert_equal [2;3;4;6;29;36]
                          (List.map fst (Dict.to_list (Dict.remove 15 dict))));
  "dup_test" >:: (fun _ -> assert_equal (Some 5)
                          (Dict.find 6 (Dict.insert 6 5 dict)));
  "rep_ok test" >:: (fun _ -> assert_equal dict 
                          (Dict.rep_ok dict));
  "empty_test" >:: (fun _ -> assert_equal true 
                          (Dict.is_empty Dict.empty));
  "string_test" >:: (fun _ -> assert_equal true 
                          (Dict1.is_empty Dict1.empty));
  "string_test_to_list" >:: (fun _ -> assert_equal ["a";"b";"c";"d";"e";"f";"g"]
                          (List.map fst (Dict1.to_list dict1)));
  "size_test" >:: (fun _ -> assert_equal 7 
                          (Dict.size dict));
  "size_test_2"  >:: (fun _ -> assert_equal 6 
                          (Dict.size (Dict.remove 6 dict)));
  "size_test_3"  >:: (fun _ -> assert_equal 7
                          (Dict.size (Dict.insert 6 3 dict)));
  "find_test"  >:: (fun _ -> assert_equal (Some 4)  
                          (Dict.find 6 dict));
  "find_test_2" >:: (fun _ -> assert_equal (Some 3)
                          (Dict.find 6 (Dict.insert 6 3 dict)));
  "member_test"  >:: (fun _ -> assert_equal true 
                          (Dict.member 6 dict));
  "member_test_2"  >:: (fun _ -> assert_equal false
                          (Dict.member 6 (Dict.remove 6 dict)));
  "choose_test"  >:: (fun _ -> assert_equal None 
                          (Dict.choose Dict.empty));
  "choose_test_2" >:: (fun _ -> assert_equal None
                  (Dict.choose (Dict.remove 1 (Dict.insert 1 1 Dict.empty))));
  "choose_test_3"  >:: (fun _ -> assert_equal (Some (6,3))
                          (Dict.choose (Dict.insert 6 3 Dict.empty)));
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