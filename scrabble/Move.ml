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
  |"Across" -> Across
  |"Down"   -> Down

  let get_move s =
  let split = Str.split (Str.regexp " +") s in
  let move = List.nth split 0 in
  match move with
  | "Play"       -> Play ( List.nth split 1,
                           string_to_direction (List.nth split 2),
                         ( String.get (List.nth split 3) 0,
                           (int_of_string (List.nth split 4))))
  | "Draw"       -> Draw
  | "SwitchAll"  -> SwitchAll
  | "SwitchSome" -> SwitchSome []
  | "Pass"       -> Pass
  | "Shuffle"    -> Shuffle


  let validate s = failwith "Unimplemented"

  (* [submit_move] enters [move] to the game and is the [state] resulting from
   * [move]'s' execution *)
  let submit_move s m = failwith "Unimplemented"

end


let rec repl c_state =
  let () = print_endline "Enter Move" in
  let s_move = read_line() in
  let new_state = HumanMove.submit_move c_state (HumanMove.get_move s_move) in
  let () = print_endline "" in
  repl new_state