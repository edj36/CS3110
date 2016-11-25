open Data

module API = struct

type state = game_state

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
  let num = Random.int sum in
  let rec helper num lst = match lst with
  | [] -> None
  | h :: t -> if (num <= h.count) && (h.count <> 0) then Some h
  else helper (num-h.count) t in helper num bag

(* [draw_letters] represents the letter list after drawing specified
 * number of letters from bag. *)
let rec draw_letters num bag =
  match num with
  | 0 -> []
  | _ -> let l = rand_char bag in
  (match l with
  | None -> failwith "Bag is enpty";
  | Some l -> add_or_draw_char l.character bag (-);
  l::draw_letters (num-1) bag)

(* [add_letters] represents the letter list after adding the letters to the list *)
let rec add_letter hands bag =
  match hands with
  | [] -> ()
  | h :: t -> add_or_draw_char h.character bag (+)

(* [current_player] represents a player who is playing on the current turn *)
let current_player (state:state) =
  let n = List.length state.player_racks in
  List.nth state.player_racks (state.turn mod n)

end
