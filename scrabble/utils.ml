open Data
open Yojson.Basic.Util

(* [get_nth] is the nth element of [lst] but instead of raising
 * exceptions (like List.nth) it raises failwith "error message"
 * This function was made with inspiration from this StackOverflow post:
 * http://stackoverflow.com/questions/9795504/return-the-nth-element-of-a-list-in-ocaml *)
let rec get_nth tup = match tup with
  | [], _ -> failwith "not in list"
  | _ , n when n < 0 -> failwith "out of bounds"
  | h::_ , 0 -> h
  | h::t , n -> get_nth (t, n-1)

(* [letter_to_char] represents chr list of input letter list*)
let letter_to_char lst =
    List.map (fun x -> x.character) lst

(* [remove] is a 'a list after removing specified element from the
* input list *)
let rec remove a lst =
  match lst with
  | [] -> []
  | h :: t -> if h = a then t else h :: (remove a t)

(* [string_to_char_list] is a char list representation of string *)
let rec string_to_char_list str =
  match str with
  | "" -> []
  | s -> (String.make 1 (String.get (String.uppercase_ascii s) 0))
  :: string_to_char_list (String.sub s 1 ((String.length s)-1))

(* [shuffle] is an 'a list after shuffling elements *)
let shuffle lst =
  Random.self_init ();
  let indexed = List.map (fun x -> (Random.bits (), x)) lst in
  let helper_sort x y = Pervasives.compare (fst x) (fst y) in
  List.map (fun x -> snd x) (List.sort helper_sort indexed)

(* [translate_coodinate] is an int*int representation of char*int coordinate*)
let translate_coodinate (x,y) =
  (y-1, (Char.code (String.get (String.lowercase_ascii x) 0) - Char.code 'a'))

(* [char_to_letter] represents letter type of input char *)
let char_to_letter c bag =
  let rec helper c bag =
  match bag with
  | [] -> None
  | h::t -> if h.character = c then Some h else helper c t in
  match helper c bag with
  | None -> failwith "No such char in the bag"
  | Some l -> l

(* [current_player] represents a player who is playing on the current turn *)
let current_player_rack state =
  let n = List.length state.player_racks in
  try get_nth (state.player_racks, (state.turn mod n)) with
  | Failure _ -> failwith "Never happens"

(* [get_nextcoodinate] is (int*int) representation of coordinate after moving
* 1 step in the specified direction from speficied origin *)
let get_nextcoordinate (x,y) dir =
  match dir with
  | Across -> if y+1 >= 0 && y+1 <= 14 then (x, y+1)
    else failwith "OutOfBoundary"
  | Down -> if x+1 >= 0 && x+1 <= 14 then (x+1, y)
    else failwith "OutOfBoundary"

(* [get_tile] represents tile of specified coordinate *)
let get_tile coordinate board =
  match coordinate with
  |(x,y) -> get_nth (get_nth (board,y) , x)

(* [crawl] is a string list representation of words, specifing the direction
 * and row/column number with [i] *)
let crawl dir i board =
  let init = match dir with Across -> (i, 0)| Down -> (0, i) in
  let rec helper dir crd acc board =
    let tile = get_tile crd board in
    let n = try get_nextcoordinate crd dir with
    | Failure _ -> (15,15) in
    if n = (15,15) then
      match tile.letter with
      | None -> acc :: []
      | Some l ->
        if acc = "" then [] else (acc ^ l) :: []
    else
      match tile.letter with
      | None ->
        if acc = "" then helper dir n "" board else acc::(helper dir n "" board)
      | Some l ->
        if acc = "" then helper dir n l board
        else helper dir n (acc ^ l) board in
  List.filter (fun x -> String.length x > 1 ) (helper dir init "" board)

(* [collect] is a string list representation of all words on the scrabble board *)
(* let collect board = *)
let collect board =
  let rec helper dir i =
    match i with
    | -1 -> []
    | _ -> (crawl dir i board) @ helper dir (i-1) in
  (helper Across 14) @ (helper Down 14)

(* [list_compare] is an 'a list representation of new elements added in a
 * new state (helper funciton for newwords, newcoordinates) *)
let rec list_compare new_l old_l = match new_l with
  | [] -> []
  | h::t -> if List.mem h old_l then list_compare t (remove h old_l)
    else h :: list_compare t old_l

(* [get_newwords] represents string list of new words created in a recent turn*)
let rec get_newwords new_w old_w = list_compare new_w old_w

(* [get_newletters] will search on board and collect the char list of
 * new letters placed on the board *)
let rec get_newletters crds new_board =
  match crds with
  | [] -> []
  | h::t -> let tile = get_tile h new_board in
    match tile.letter with
    |Some c -> c :: (get_newletters t new_board)
    |None -> get_newletters t new_board

(* [subst] will substitute element a into nth index of lst*)
let rec subst lst n a =
  match lst with
  | [] -> []
  | h::t -> if n = 0 then a::t else h::(subst t (n-1) a)

(* [fill_coodinate] is an updated game board after filling the specified
 * coordinates with element, [fill] *)
let rec fill_coordinate coordinates fill board =
  match coordinates with
  |[]-> board
  |(x,y)::t ->
    let temp = get_nth (board, y) in
    fill_coordinate t fill (subst board y (subst temp x fill))

(* [place_string] will place the string on the board given, word,
 * starting coordinate, direction, and current board *)
let rec place_string str dir crd board =
  match String.length str with
  | 0 -> board
  | n ->
    if crd = (15,15) then board else
    let chr = String.make 1 (String.get (String.uppercase_ascii str) 0) in
    let tile = get_tile crd board in
    let new_tile = match tile.letter with
      | Some c -> if c = chr then {bonus = tile.bonus; letter = Some chr}
        else failwith "You Cannot Override exsiting character"
      | None -> {bonus = tile.bonus; letter = Some chr} in
    let update = fill_coordinate [crd] new_tile board in
    let next = try get_nextcoordinate crd dir with
      | Failure _ -> (15,15) in
    place_string (String.sub str 1 (String.length str - 1)) dir next update

(* [collect_coordinates] is a (int*int) list representation of occupied
 * coordinates on the current board *)
let collect_coordinates board =
  let rec help_a i1 board =
    match i1 with
    | -1 -> []
    | _ ->
    let rec help_d i1 i2 board =
      match i2 with
      | -1 -> []
      | _ -> let tile = get_tile (i1,i2) board in
        (match tile.letter with
          | None -> help_d i1 (i2-1) board
          | Some _ -> (i1,i2) :: help_d i1 (i2-1) board) in
    help_d i1 14 board @ help_a (i1-1) board in
  help_a 14 board

(* [word_score] is an int representation of raw score of word *)
let rec word_score str state =
  match str with
  | "" -> 0
  | _ ->
  let letter = char_to_letter (String.make 1 (String.get str 0)) state.letter_bag in
  letter.pt + word_score (String.sub str 1 ((String.length str)-1)) state

(* [get_newcoordinates] is a (int*int) list representation of all new words
 * made in the most recent turn *)
let get_newcoordinates new_l old_l = list_compare new_l old_l

(* [print_message] will parse json file and print message stoared in a json
 * file.
 * [name] : string, name of json member *)
let print_message name =
  let src = Yojson.Basic.from_file "info.json" in
  let message = src |> member name |> to_string in
  print_string message

(* [print_score] is a string type of score board. Take a current score_board
 * and prints player's name followed by the score
 * input: game.score_board *)
let rec print_score = function
  | []-> ()
  | (x,y)::t -> let name = (match x with
    |Human n1 -> n1
    |AI (n2,i) -> n2) in
  print_string (name ^ ": " ^ (string_of_int y) ^ " \n");
  print_score t

(********** TESTING TOOLS **********)
let rec get_score state name =
  match state.score_board with
  | [] -> 0
  | (p, score) :: t ->
    let n = (match p with Human n1 -> n1 |AI (n2,i) -> n2) in
    if n = name then score else get_score state name
