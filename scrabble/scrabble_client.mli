(*

(* [get_state] will be called during the "busy wait" that each player 
 * does when its not their turn. The function always sends the same 
 * GET request to the server and returns the state object the 
 * server currently has. The player can then check this object to see 
 * if its their turn. *)
val get_state : Data.game_state

(* [post_move] sends the [move] and current [state] to the server via a 
 * POST request, and represents the new state resulting from executing 
 * [move] in [state] *)
val post_move : Data.move -> Data.game_state -> Data.game_state

(* [new_game]*)

*)

