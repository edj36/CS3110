
type letter = { character: char; pt : int; count : int}

(* [initial_bag] is a letter list representing the intial state of the game *)
let initialize_bag ()=
[
  {character = 'A'; pt = 1; count = 9};
  {character = 'B'; pt = 3; count = 2};
  {character = 'C'; pt = 3; count = 2};
  {character = 'D'; pt = 2; count = 4};
  {character = 'E'; pt = 1; count = 12};
  {character = 'F'; pt = 4; count = 2};
  {character = 'G'; pt = 2; count = 3};
  {character = 'H'; pt = 4; count = 2};
  {character = 'I'; pt = 1; count = 9};
  {character = 'J'; pt = 8; count = 1};
  {character = 'K'; pt = 5; count = 1};
  {character = 'L'; pt = 1; count = 4};
  {character = 'M'; pt = 3; count = 2};
  {character = 'N'; pt = 1; count = 6};
  {character = 'O'; pt = 1; count = 8};
  {character = 'P'; pt = 3; count = 2};
  {character = 'Q'; pt = 10;count = 1};
  {character = 'R'; pt = 1; count = 6};
  {character = 'S'; pt = 1; count = 4};
  {character = 'T'; pt = 1; count = 6};
  {character = 'U'; pt = 1; count = 4};
  {character = 'V'; pt = 4; count = 2};
  {character = 'W'; pt = 4; count = 2};
  {character = 'X'; pt = 8; count = 1};
  {character = 'Y'; pt = 4; count = 2};
  {character = 'Z'; pt = 10;count = 1};
  {character = ' '; pt = 0; count = 2}
]

(* [draw_char] is a letter list representing the state
 * after drawing a letter from the list *)
let rec draw_char c bag =
  match bag with
  |[]-> bag
  |h::t ->
  if h.character = c then
   match (h.count) with
   | 0 -> h::t
   | _ -> {character = h.character; pt = h.pt; count = h.count-1} :: t
  else h :: draw_char c t

(* [rand_char] is a letter option which is simulated after randomly pick
 * one letter from the letter bag *)
let rand_char bag =
  let sum = List.fold_left (fun acc elm -> acc + elm.count) 0 bag in
  let num = Random.int sum in
  let rec helper num lst = match lst with
  | [] -> None
  | h :: t -> if num <= h.count then Some h else helper (num-h.count) t
  in helper num bag

(* [initial_hand] is a letter list of 7 randomly chosen letters *)
let initial_hand () =
  let bag = initialize_bag () in
  let rec helper num bag =
    if num = 0 then []
    else match (rand_char bag) with
    | None -> []
    | Some h -> let new_bag = draw_char h.character bag in
    h :: helper (num-1) new_bag in
  helper 7 bag
