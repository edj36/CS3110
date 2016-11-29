open Data

(* [get_nth] is the nth element of [lst] but instead of raising
 * exceptions (like List.nth) it raises failwith "error message"
 * This function was made with inspiration from this StackOverflow post:
 * http://stackoverflow.com/questions/9795504/return-the-nth-element-of-a-list-in-ocaml
 *)
let rec get_nth tup = match tup with
  | [], _ -> failwith "not in list"
  | _ , n when n < 0 -> failwith "out of bounds"
  | h::_ , 0 -> h
  | h::t , n -> get_nth (t, n-1)

(* [letter_to_char] represents chr list of input letter list*)
let letter_to_char lst =
    List.map (fun x -> x.character) lst

(* [string_to_direction] is a type direction representation
 * of string type input [s] *)
let string_to_direction s =
  match s with
  | "Across" | "a" | "across"-> Across
  | "Down" | "d" | "down" -> Down
  | _ -> failwith "Invaild direction"

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
  | s -> Char.uppercase_ascii (String.get s 0)
  ::string_to_char_list (String.sub s 1 ((String.length s)-1))

(* [shuffle] is an 'a list after shuffling elements *)
let shuffle lst =
  Random.self_init ();
  let indexed = List.map (fun x -> (Random.bits (), x)) lst in
  let helper_sort x y = Pervasives.compare (fst x) (fst y) in
  List.map (fun x -> snd x) (List.sort helper_sort indexed)

(* [char_to_letter] represents letter type of input char *)
let char_to_letter c bag =
  let rec helper c bag =
  match bag with
  | [] -> None
  | h::t -> if h.character = c then Some h else helper c t in
  match helper c bag with
  | None -> failwith "No such char in the bag"
  | Some l -> l

(* [add_or_draw_char] is a letter list representing the state
* after adding or drawing a letter from the list
* [op] : either (+) or (-) *)
let rec add_or_draw_char c bag op =
  match bag with
  | []-> ()
  | h::t ->
  if h.character = c then h.count <- op h.count 1
  else add_or_draw_char c t op

(* [rand_char] is a letter option which is simulated after randomly pick
* one letter from the letter bag *)
let rand_char bag =
  let sum = List.fold_left (fun acc elm -> acc + elm.count) 0 bag in
  match sum with
  | 0 -> failwith "Bag is empty!"
  | _ ->
  Random.self_init ();
  let num = Random.int sum in
  let rec helper num lst = match lst with
  | [] -> None
  | h :: t -> if (num <= h.count) && (h.count <> 0) then Some h
  else helper (num-h.count) t in helper num bag

(* [draw_letters] represents the letter list after drawing specified
* number of letters from bag. *)
let rec draw_letters num bag =
  let sum = List.fold_left (fun a e -> a + e.count) 0 bag in
  if sum >= num then
    match num with
    | 0 -> []
    | _ -> let l = rand_char bag in
    (match l with
    | None -> failwith "Bag is enpty";
    | Some l -> add_or_draw_char l.character bag (-);
    l::draw_letters (num-1) bag)
  else draw_letters sum bag

(* [add_letters] represents the letter list after adding the letters to the list *)
let rec add_letter hands bag =
  match hands with
  | [] -> ()
  | h :: t -> add_or_draw_char h.character bag (+); add_letter t bag

(* [current_player] represents a player who is playing on the current turn *)
let current_player_rack state =
  let n = List.length state.player_racks in
  try get_nth (state.player_racks, (state.turn mod n)) with
  | Failure _ -> failwith "Never happens"

(* let rec current_player_rack state = match state.player_racks with
  | [] -> failwith "Never happens"
  | h :: t when h =  *)

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
      | None -> []
      | Some l ->
        if acc = "" then [] else (acc ^ (Char.escaped l)) :: []
    else
      match tile.letter with
      | None ->
        if acc = "" then helper dir n "" board else acc::(helper dir n "" board)
      | Some l ->
        if acc = "" then helper dir n (Char.escaped l) board
        else helper dir n (acc ^ (Char.escaped l)) board in
  List.filter (fun x -> String.length x <> 1 ) (helper dir init "" board)

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
  let letter = char_to_letter (String.get str 0) state.letter_bag in
  letter.pt + word_score (String.sub str 1 ((String.length str)-1)) state

(* [get_newcoordinates] is a (int*int) list representation of all new words
 * made in the most recent turn *)
let get_newcoordinates new_l old_l = list_compare new_l old_l
