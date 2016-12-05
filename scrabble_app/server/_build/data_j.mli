(* Auto-generated from "data.atd" *)


type bonus_status = Data_t.bonus_status

type tile = Data_t.tile = { bonus: bonus_status; letter: string option }

type scrabble_board = Data_t.scrabble_board

type player = Data_t.player

type score_board = Data_t.score_board

type letter = Data_t.letter = {
  character: string;
  pt: int;
  mutable count: int
}

type player_rack = Data_t.player_rack

type direction = Data_t.direction

type coordinate = Data_t.coordinate

type play = Data_t.play = {
  word: string;
  direction: direction;
  coordinate: coordinate
}

type move = Data_t.move

type letter_bag = Data_t.letter_bag

type game_state = Data_t.game_state = {
  board: scrabble_board;
  score_board: score_board;
  letter_bag: letter_bag;
  player_racks: player_rack list;
  turn: int;
  counter: int;
  quit: bool
}

val write_bonus_status :
  Bi_outbuf.t -> bonus_status -> unit
  (** Output a JSON value of type {!bonus_status}. *)

val string_of_bonus_status :
  ?len:int -> bonus_status -> string
  (** Serialize a value of type {!bonus_status}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_bonus_status :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> bonus_status
  (** Input JSON data of type {!bonus_status}. *)

val bonus_status_of_string :
  string -> bonus_status
  (** Deserialize JSON data of type {!bonus_status}. *)

val write_tile :
  Bi_outbuf.t -> tile -> unit
  (** Output a JSON value of type {!tile}. *)

val string_of_tile :
  ?len:int -> tile -> string
  (** Serialize a value of type {!tile}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tile :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tile
  (** Input JSON data of type {!tile}. *)

val tile_of_string :
  string -> tile
  (** Deserialize JSON data of type {!tile}. *)

val write_scrabble_board :
  Bi_outbuf.t -> scrabble_board -> unit
  (** Output a JSON value of type {!scrabble_board}. *)

val string_of_scrabble_board :
  ?len:int -> scrabble_board -> string
  (** Serialize a value of type {!scrabble_board}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_scrabble_board :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> scrabble_board
  (** Input JSON data of type {!scrabble_board}. *)

val scrabble_board_of_string :
  string -> scrabble_board
  (** Deserialize JSON data of type {!scrabble_board}. *)

val write_player :
  Bi_outbuf.t -> player -> unit
  (** Output a JSON value of type {!player}. *)

val string_of_player :
  ?len:int -> player -> string
  (** Serialize a value of type {!player}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_player :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> player
  (** Input JSON data of type {!player}. *)

val player_of_string :
  string -> player
  (** Deserialize JSON data of type {!player}. *)

val write_score_board :
  Bi_outbuf.t -> score_board -> unit
  (** Output a JSON value of type {!score_board}. *)

val string_of_score_board :
  ?len:int -> score_board -> string
  (** Serialize a value of type {!score_board}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_score_board :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> score_board
  (** Input JSON data of type {!score_board}. *)

val score_board_of_string :
  string -> score_board
  (** Deserialize JSON data of type {!score_board}. *)

val write_letter :
  Bi_outbuf.t -> letter -> unit
  (** Output a JSON value of type {!letter}. *)

val string_of_letter :
  ?len:int -> letter -> string
  (** Serialize a value of type {!letter}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_letter :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> letter
  (** Input JSON data of type {!letter}. *)

val letter_of_string :
  string -> letter
  (** Deserialize JSON data of type {!letter}. *)

val write_player_rack :
  Bi_outbuf.t -> player_rack -> unit
  (** Output a JSON value of type {!player_rack}. *)

val string_of_player_rack :
  ?len:int -> player_rack -> string
  (** Serialize a value of type {!player_rack}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_player_rack :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> player_rack
  (** Input JSON data of type {!player_rack}. *)

val player_rack_of_string :
  string -> player_rack
  (** Deserialize JSON data of type {!player_rack}. *)

val write_direction :
  Bi_outbuf.t -> direction -> unit
  (** Output a JSON value of type {!direction}. *)

val string_of_direction :
  ?len:int -> direction -> string
  (** Serialize a value of type {!direction}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_direction :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> direction
  (** Input JSON data of type {!direction}. *)

val direction_of_string :
  string -> direction
  (** Deserialize JSON data of type {!direction}. *)

val write_coordinate :
  Bi_outbuf.t -> coordinate -> unit
  (** Output a JSON value of type {!coordinate}. *)

val string_of_coordinate :
  ?len:int -> coordinate -> string
  (** Serialize a value of type {!coordinate}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_coordinate :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> coordinate
  (** Input JSON data of type {!coordinate}. *)

val coordinate_of_string :
  string -> coordinate
  (** Deserialize JSON data of type {!coordinate}. *)

val write_play :
  Bi_outbuf.t -> play -> unit
  (** Output a JSON value of type {!play}. *)

val string_of_play :
  ?len:int -> play -> string
  (** Serialize a value of type {!play}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_play :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> play
  (** Input JSON data of type {!play}. *)

val play_of_string :
  string -> play
  (** Deserialize JSON data of type {!play}. *)

val write_move :
  Bi_outbuf.t -> move -> unit
  (** Output a JSON value of type {!move}. *)

val string_of_move :
  ?len:int -> move -> string
  (** Serialize a value of type {!move}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_move :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> move
  (** Input JSON data of type {!move}. *)

val move_of_string :
  string -> move
  (** Deserialize JSON data of type {!move}. *)

val write_letter_bag :
  Bi_outbuf.t -> letter_bag -> unit
  (** Output a JSON value of type {!letter_bag}. *)

val string_of_letter_bag :
  ?len:int -> letter_bag -> string
  (** Serialize a value of type {!letter_bag}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_letter_bag :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> letter_bag
  (** Input JSON data of type {!letter_bag}. *)

val letter_bag_of_string :
  string -> letter_bag
  (** Deserialize JSON data of type {!letter_bag}. *)

val write_game_state :
  Bi_outbuf.t -> game_state -> unit
  (** Output a JSON value of type {!game_state}. *)

val string_of_game_state :
  ?len:int -> game_state -> string
  (** Serialize a value of type {!game_state}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_game_state :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> game_state
  (** Input JSON data of type {!game_state}. *)

val game_state_of_string :
  string -> game_state
  (** Deserialize JSON data of type {!game_state}. *)

