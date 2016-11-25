open Data

module API = struct
(* [draw_char] is a letter list representing the state
 * after drawing a letter from the list *)
let rec draw_char c bag =
  match bag with
  | []-> ()
  | h::t ->
  if h.character = c then h.count <- h.count - 1
  else draw_char c t

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

(* [draw_letters] represents the letter option list after drawing specified
 * number of letters from bag. *)
let rec draw_letters num bag =
  match num with
  | 0 -> []
  | _ -> let l = rand_char bag in
  (match l with
  | None -> failwith "Bag is enpty";
  | Some l -> draw_char l.character bag); l::draw_letters (num-1) bag

(* [current_player] represents a player who is playing on the current turn *)
let current_player state =
  let n = List.length state.player_racks in
  match List.nth state.player_racks (state.turn mod n) with
  (x,y) -> x
end
