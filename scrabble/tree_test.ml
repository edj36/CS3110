open OUnit2
open Tree

let tests = [

  "Tree Test 1"   >:: (fun _ -> assert_equal true  (Tree.find_word "AA") );
  "Tree Test 2"   >:: (fun _ -> assert_equal false (Tree.find_word "") );
  "Tree Test 3"   >:: (fun _ -> assert_equal true  (Tree.find_word "AB") );
  "Tree Test 4"   >:: (fun _ -> assert_equal true  (Tree.find_word "COMPUTER"));
  "Tree Test 5"   >:: (fun _ -> assert_equal true  (Tree.find_word "SCIENCE"));
  "Tree Test 6"   >:: (fun _ -> assert_equal true  (Tree.find_word "SCIENCES"));
  "Tree Test 7"   >:: (fun _ -> assert_equal false (Tree.find_word "ZZZZZ"));
  "Tree Test 8"   >:: (fun _ -> assert_equal false (Tree.find_word "aa"));
  "Tree Test 9"   >:: (fun _ -> assert_equal false (Tree.find_word "   "));
  "Tree Test 10"  >:: (fun _ -> assert_equal false (Tree.find_word "OCAML"));


]