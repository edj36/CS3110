open RadixTree
open PString
exception Not_found


(*[read_loop] is the tree created from adding the lines of [channel] into tree
[t] until the end of the file
requires:
* [channel] to be a valid in-channel from opening a file
* [t] o to be a valid tree*)
let read_loop channel t =
 let mutabletree = ref (Node(MapChar.empty, [])) in
 try
  while true; do
   let word = input_line (channel) in
   let tree = PString.add word true !mutabletree in
    mutabletree := tree
  done; !mutabletree
 with End_of_file ->
 close_in channel; !mutabletree


(*[text_read] is the tree with a [file]'s lines added into the
tree [t] as strings, with an initial empty tree
requires:
* [file] to be a valid file with each line as a word*)
let text_read file =
  let channel = open_in file in
  read_loop channel empty

(*[sd] is the tree initialized to the scrabble dictionary *)
let sd = text_read "dictionary.txt"

(*[find_word] is true if [str] is in tree [t] and false otherwise
requires:
*[str] to be a string in uppercase form
* t]   to be a valid radix tree*)
let find_word str =
 try
   find str sd
 with _-> false
