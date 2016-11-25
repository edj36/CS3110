open Data
open String
open Str
open List


module type Move = sig
  type state
  type move
  val get_move : string -> move
  val submit_move: state -> move -> state
  val validate : string -> bool
end

module HumanMove : (Move with type state = game_state) =  struct

  (* type for game state *)
  type state = game_state

  (* type for the move player makes *)
  type move = moves

  let string_to_direction s =
    match s with
    | "Across" | "a" -> Across
    | "Down" | "d"  -> Down
    | _ -> failwith "Invaild direction"

  let check_coordinate (x,y) =
    let lower_x = Char.lowercase_ascii x in
     (0 <= y) && (y <= 14)
     && (Char.code lower_x >= Char.code 'a')
     && (Char.code 'o' >= Char.code lower_x)

  let get_move s =
    let split = Str.split (Str.regexp " +") (s ^ " ") in
    let move = List.nth split 0 in
    let n = List.length split in
    match move with
    | "Play" | "play" | "p" ->
      let coordinate = (String.get (List.nth split 3) 0,
      (int_of_string (List.nth split 4))) in
      if (check_coordinate coordinate) && n = 5 then
      Play
      {
        word = List.nth split 1;
        direction = string_to_direction (List.nth split 2);
        coordinate = coordinate
      }
      else failwith "Invalid coordinate"
    | "Draw" | "draw" | "d" ->
      if n = 1 then Draw
      else failwith "Invalid command"
    | "SwitchAll" | "Switch_All" | "sa" | "s_a" ->
      if n = 1 then SwitchAll else failwith "Invalid command"
    | "SwitchSome" | "switchsome" | "s" | "s_s"->
      if n >= 2 then
      let char_lst = List.map (fun x -> String.get x 0) (List.tl split) in
      SwitchSome char_lst else failwith "Invalid command"
    | "Pass" | "pass" -> if n = 1 then Pass else failwith "Unimplemented"
    | "Shuffle" | "shuffle" -> if n = 1 then Shuffle else failwith "Unimplemented"
    | _ -> failwith "Invalid command"

  let validate s = failwith "Unimplemented"

  (* [submit_move] enters [move] to the game and is the [state] resulting from
   * [move]'s' execution *)
  let submit_move (s :state) (m:move) =
    match m with
    | Play {word = str; direction = dir; coordinate = crd}
      -> s
    | Draw -> failwith "Unimplemented"
    | SwitchAll -> failwith "Unimplemented"
    | SwitchSome _ -> failwith "Unimplemented"
    | Pass -> failwith "Unimplemented"
    | Shuffle -> failwith "Unimplemented"
end
