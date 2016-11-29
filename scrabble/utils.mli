
(*******************  useful tools  *******************)

(* [get_nth] is the nth element of [lst] but instead of raising
 * exceptions (like List.nth) it raises failwith "error message"
 * This function was made with inspiration from this StackOverflow post:
 * http://stackoverflow.com/questions/9795504/return-the-nth-element-of-a-list-in-ocaml
 *)
val get_nth: 'a list * int -> 'a

(* [letter_to_char] represents chr list of input letter list *)
val letter_to_char : Data.letter list -> char list

(* [string_to_direction] is a type direction representation of input string *)
val string_to_direction : string -> Data.direction

(* [remove] represents 'a list after removing specified element from the
 * input list *)
val remove : 'a -> 'a list -> 'a list

(* [string_to_char_list] is a char list representation of string *)
val string_to_char_list : string -> char list

(* [shuffle] is an 'a list after shuffling elements *)
val shuffle : 'a list -> 'a list

(*******************  update letter bag  *******************)

(* [char_to_letter] represents letter type of input char *)
val char_to_letter : char -> Data.letter list -> Data.letter

(* [draw_letters] represents specified length of letter list drawn from
 * letter bag. letter bag will update its field *)
val draw_letters : int -> Data.letter list -> Data.letter list

(* [add_letters] represents unit resulting from updating the field of letter bag
 * after adding letter list to the letter bag *)
val add_letter : Data.letter list -> Data.letter list -> unit

(*******************  update state  *******************)

(* [current_player_rack] represents the player_rack of current player_rack*)
val current_player_rack : Data.game_state -> Data.player_rack

(* [get_nextcoodinate] is (int*int) representation of coordinate after moving
 * 1 step in the specified direction from speficied origin *)
val get_nextcoordinate : (int * int) -> Data.direction -> (int * int)

(* [get_tile] represents tile of specified coordinate *)
val get_tile : (int * int) -> Data.scrabble_board -> Data.tile

(* [fill_coordinate] is a tile array array representation of game board.
 * after filling tile at specified coordinate *)
val fill_coordinate : (int * int) list -> Data.tile -> Data.scrabble_board -> Data.scrabble_board

(* [crawl] is a string list representation of words, specifing the direction
 * and row/column number with [i] *)
val crawl : Data.direction -> int -> Data.scrabble_board -> string list

(* [collect] is a string list representation of all words on the scrabble board *)
val collect : Data.scrabble_board -> string list

(* [get_newwords] is a string list representation of all new words made in the
 * most recent turn *)
val get_newwords : string list -> string list -> string list

(*******************  SCORING  *******************)

(* [collect_coordinates] is a (int*int) list representation of occupied
 * coordinates on the current board *)
val collect_coordinates : Data.scrabble_board-> (int*int) list

(* [word_score] is a score of string type input word *)
val word_score: string -> Data.game_state -> int

(* [get_newcoordinates] is a (int*int) list representation of all new words
 * made in the most recent turn *)
val get_newcoordinates : (int*int) list -> (int*int) list -> (int*int) list
