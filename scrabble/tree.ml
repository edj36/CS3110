open RadixTree
open PString
exception Not_found

let rec rec_read channel t =
try
  let word = input_line (channel) in
  let tree = PString.add word true t in
  rec_read channel tree
with End_of_file ->
close_in channel; t


let text_read file =
  let channel = open_in file in
  rec_read channel empty


let find_word str t =
 try
   find str t
 with _-> false
