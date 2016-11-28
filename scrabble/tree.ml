open RadixTree
open PString
exception Not_found

let rec rec_read channel t =

let mutabletree = ref (Node(MapChar.empty, [])) in

try
  while true; do
  let word = input_line (channel) in
  let tree = PString.add word true !mutabletree in
  mutabletree := tree
done; !mutabletree

with End_of_file ->
close_in channel; !mutabletree


let text_read file =
  let channel = open_in file in
  rec_read channel empty


let find_word str t =
 try
   find str t
 with _-> false


let sd = text_read "dictionary.txt"