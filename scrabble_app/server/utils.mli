
(*******************  useful tools  *******************)

(* [get_nth] is the nth element of [lst] but instead of raising
 * exceptions (like List.nth) it raises failwith "error message"
 * This function was made with inspiration from this StackOverflow post:
 * http://stackoverflow.com/questions/9795504/return-the-nth-element-of-a-list-in-ocaml
 *)
val get_nth: 'a list * int -> 'a

(* [letter_to_char] represents chr list of input letter list *)
val letter_to_char : Data_t.letter list -> string list

(* [remove] represents 'a list after removing specified element from the
 * input list *)
val remove : 'a -> 'a list -> 'a list

(* [string_to_char_list] is a char list representation of string *)
val string_to_char_list : string -> string list

(* [shuffle] is an 'a list after shuffling elements *)
val shuffle : 'a list -> 'a list

(* [translate_coodinate] is an int*int representation of char*int coordinate*)
val translate_coodinate : (string * int) -> (int * int)

(* [char_to_letter] represents letter type of input char *)
val char_to_letter : string -> Data_t.letter list -> Data_t.letter

(*******************  update state  *******************)

(* [current_player_rack] represents the player_rack of current player_rack*)
val current_player_rack : Data_t.game_state -> Data_t.player_rack

(* [get_nextcoodinate] is (int*int) representation of coordinate after moving
 * 1 step in the specified direction from speficied origin *)
val get_nextcoordinate : (int * int) -> Data_t.direction -> (int * int)

(* [get_tile] represents tile of specified coordinate *)
val get_tile : (int * int) -> Data_t.scrabble_board -> Data_t.tile

(* [fill_coordinate] is a tile array array representation of game board.
 * after filling tile at specified coordinate *)
val fill_coordinate : (int * int) list -> Data_t.tile -> Data_t.scrabble_board -> Data_t.scrabble_board

(* [crawl] is a string list representation of words, specifing the direction
 * and row/column number with [i] *)
val crawl : Data_t.direction -> int -> Data_t.scrabble_board -> string list

(* [collect] is a string list representation of all words on the scrabble board *)
val collect : Data_t.scrabble_board -> string list

(* [get_newwords] is a string list representation of all new words made in the
 * most recent turn *)
val get_newwords : string list -> string list -> string list

(* [get_newletters] comapares old board with new board and returns the list of
 * new characters *)
val get_newletters : (int*int) list -> Data_t.scrabble_board -> string list

(* [place_string] returns updated board after placing specified string on the board
 * with specified initial coordinate and direction *)
val place_string : string -> Data_t.direction -> (int*int) -> Data_t.scrabble_board -> Data_t.scrabble_board
(*******************  SCORING  *******************)

(* [collect_coordinates] is a (int*int) list representation of occupied
 * coordinates on the current board *)
val collect_coordinates : Data_t.scrabble_board-> (int*int) list

(* [word_score] is a score of string type input word *)
val word_score : string -> Data_t.game_state -> int

(* [get_newcoordinates] is a (int*int) list representation of all new words
 * made in the most recent turn *)
val get_newcoordinates : (int*int) list -> (int*int) list -> (int*int) list

(********** TESTING TOOLS **********)

(* [get_score] is an int type representation of the score of specified player *)
val get_score : Data_t.game_state -> string -> int
