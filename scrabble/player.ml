open String
open Str
open Utils
open Data

module type Player = sig

  (* type for game state *)
  type t
  (* type for move *)
  type m

  (* [make_move] is the [move] based on user input and the [move] in
   * in progress:
   * first [int] is x coordinate of [letter]
   * second [int] is y coordinate of [letter]
   * [letter] is letter being put on the board at the above coordinates
   * [make_move] adds the coordinate-letter combination to the list of
   * existing coordinate-letter combinations already inside of the
   * argument [move]
   * Requires:
   * [m] is of type Move within the move variant *)
  val get_move : t -> m 

end

module Human : (Player) = struct

  (* type for game state *)
  type t = string
  (* type for the move player makes *)
  type m = move

  (* [check_coordinate] is a bool indicating the validity of input coordinate
   * false means the invalid input for the coodinate, true mweans the coodinate
   * is valid *)
  let check_coordinate (x,y) =
    let lower_x = Char.lowercase_ascii x in
     (0 <= y) && (y <= 14)
     && (Char.code lower_x >= Char.code 'a')
     && (Char.code 'o' >= Char.code lower_x)

  let check_chars lst state =
    let player = current_player state in
    let letters = List.map (fun x -> char_to_letter x state.letter_bag) lst in
    List.fold_left (fun acc x-> (List.mem x (snd player)) && acc) true letters

  let get_move s =
    let split = Str.split (Str.regexp " +") (s ^ " ") in
    let move = List.nth split 0 in
    let n = List.length split in
    match move with
    | "Play" | "play" | "p" ->
      let coordinate = (String.get (List.nth split 3) 0,
      (int_of_string (List.nth split 4))) in
      if n = 5 && (check_coordinate coordinate) then
      Play
      {
        word = List.nth split 1;
        direction = string_to_direction (List.nth split 2);
        coordinate = coordinate
      }
      else failwith "Invalid coordinate"
    | "SwitchAll" | "Switch_All" | "sa" | "s_a" ->
      if n = 1 then SwitchAll
      else failwith "Invalid command"
    | "SwitchSome" | "switchsome" | "s" | "s_s"->
      if n >= 2 then
        let char_lst = List.map (fun x -> String.get x 0) (List.tl split) in SwitchSome char_lst
      else failwith "Invalid command"
    | "Pass" | "pass" -> if n = 1 then Pass else failwith "Invalid command"
    | "Shuffle" | "shuffle" -> if n = 1 then Shuffle else failwith "Invalid command"
    | _ -> failwith "Invalid command"

end


module AI : (Player) =  struct

  (* type for game state *)
  type t = game_state
  (* type for the move player makes *)
  type m = move

  (*type d = Data.direction*)

  let string_to_direction s = failwith "Unimplemented"

  let get_move s = failwith "Unimplemented"

end
