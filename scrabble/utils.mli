
module Utils : sig 

	type state

	(* Data.letter *)
	type l 

	type p_r

	type d

	type t

	(*******************  useful tools  *******************)

	(* [remove] represents 'a list after removing specified element from the
	 * input list *)
	val remove : 'a -> 'a list -> 'a list

	(* [string_to_char_list] is a char list representation of string *)
	val string_to_char_list : string -> char list

	(*******************  update letter bag  *******************)

	(* [char_to_letter] represents letter type of input char *)
	val char_to_letter : char -> l list -> l

	(* [letter_to_char] represents chr list of input letter list*)
	val letter_to_char : l list -> char list

	(* [draw_letters] represents specified length of letter list drawn from
	 * letter bag. letter bag will update its field *)
	val draw_letters : int -> l list -> l list

	(* [add_letters] represents unit resulting from updating the field of letter bag
	 * after adding letter list to the letter bag *)
	val add_letter : l list -> l list -> unit

	(*******************  update state  *******************)

	(* [current_player] represents the player_rack of current player *)
	val current_player : state -> p_r

	(* [get_nextcoodinate] is (int*int) representation of coordinate after moving
	 * 1 step in the specified direction from speficied origin *)
	val get_nextcoordinate : (int * int) -> d -> (int * int)

	(* [get_tile] represents tile of specified coordinate *)
	val get_tile : (int * int) -> t array array -> t

	(* [fill_coordinate] is a tile array array representation of game board.
	 * after filling tile at specified coordinate *)
	val fill_coordinate : (int * int) list -> t -> t array array

	(* [collect] is a string list representation of all words on the scrabble board *)
	val collect : t array array -> string list

	(* [get_newwords] is a string list representation of all new words made in the
	 * most recent turn *)
	val get_newwords : state -> string list
	
end
