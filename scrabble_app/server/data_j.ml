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

let write_bonus_status = (
  fun ob sum ->
    match sum with
      | `Double_letter -> Bi_outbuf.add_string ob "<\"double_letter\">"
      | `Double_word -> Bi_outbuf.add_string ob "<\"double_word\">"
      | `Triple_letter -> Bi_outbuf.add_string ob "<\"triple_letter\">"
      | `Triple_word -> Bi_outbuf.add_string ob "<\"triple_word\">"
      | `Center -> Bi_outbuf.add_string ob "<\"center\">"
      | `Normal -> Bi_outbuf.add_string ob "<\"normal\">"
)
let string_of_bonus_status ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_bonus_status ob x;
  Bi_outbuf.contents ob
let read_bonus_status = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 6 -> (
                      match String.unsafe_get s pos with
                        | 'c' -> (
                            if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' then (
                              4
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'n' -> (
                            if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'l' then (
                              5
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | 11 -> (
                      match String.unsafe_get s pos with
                        | 'd' -> (
                            if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'b' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'd' then (
                              1
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 't' -> (
                            if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'd' then (
                              3
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | 13 -> (
                      match String.unsafe_get s pos with
                        | 'd' -> (
                            if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'b' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'r' then (
                              0
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 't' -> (
                            if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'r' then (
                              2
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Double_letter
            | 1 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Double_word
            | 2 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Triple_letter
            | 3 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Triple_word
            | 4 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Center
            | 5 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Normal
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 6 -> (
                      match String.unsafe_get s pos with
                        | 'c' -> (
                            if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' then (
                              4
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 'n' -> (
                            if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'l' then (
                              5
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | 11 -> (
                      match String.unsafe_get s pos with
                        | 'd' -> (
                            if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'b' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'd' then (
                              1
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 't' -> (
                            if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'w' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'd' then (
                              3
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | 13 -> (
                      match String.unsafe_get s pos with
                        | 'd' -> (
                            if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'b' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'r' then (
                              0
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | 't' -> (
                            if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'e' && String.unsafe_get s (pos+12) = 'r' then (
                              2
                            )
                            else (
                              raise (Exit)
                            )
                          )
                        | _ -> (
                            raise (Exit)
                          )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              `Double_letter
            | 1 ->
              `Double_word
            | 2 ->
              `Triple_letter
            | 3 ->
              `Triple_word
            | 4 ->
              `Center
            | 5 ->
              `Normal
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | _ -> (
                assert false
              )
        )
)
let bonus_status_of_string s =
  read_bonus_status (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__2 = (
  Ag_oj_run.write_option (
    Yojson.Safe.write_string
  )
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 then (
                  match String.unsafe_get s pos with
                    | 'N' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | 'S' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          1
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | _ -> (
                        raise (Exit)
                      )
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | 1 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  Ag_oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'N' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              (None : _ option)
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Ag_oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_tile : _ -> tile -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"bonus\":";
    (
      write_bonus_status
    )
      ob x.bonus;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"letter\":";
    (
      write__2
    )
      ob x.letter;
    Bi_outbuf.add_char ob '}';
)
let string_of_tile ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_tile ob x;
  Bi_outbuf.contents ob
let read_tile = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_bonus = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_letter = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 5 -> (
                if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_bonus := (
              (
                read_bonus_status
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_letter := (
              (
                read__2
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 5 -> (
                  if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_bonus := (
                (
                  read_bonus_status
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_letter := (
                (
                  read__2
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields p [| !bits0 |] [| "bonus"; "letter" |];
        (
          {
            bonus = !field_bonus;
            letter = !field_letter;
          }
         : tile)
      )
)
let tile_of_string s =
  read_tile (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  Ag_oj_run.write_list (
    write_tile
  )
)
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let read__3 = (
  Ag_oj_run.read_list (
    read_tile
  )
)
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__4 = (
  Ag_oj_run.write_list (
    write__3
  )
)
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let read__4 = (
  Ag_oj_run.read_list (
    read__3
  )
)
let _4_of_string s =
  read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_scrabble_board = (
  write__4
)
let string_of_scrabble_board ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_scrabble_board ob x;
  Bi_outbuf.contents ob
let read_scrabble_board = (
  read__4
)
let scrabble_board_of_string s =
  read_scrabble_board (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_player = (
  fun ob sum ->
    match sum with
      | `Human x ->
        Bi_outbuf.add_string ob "<\"human\":";
        (
          Yojson.Safe.write_string
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | `AI x ->
        Bi_outbuf.add_string ob "<\"ai\":";
        (
          fun ob x ->
            Bi_outbuf.add_char ob '(';
            (let x, _ = x in
            (
              Yojson.Safe.write_string
            ) ob x
            );
            Bi_outbuf.add_char ob ',';
            (let _, x = x in
            (
              Yojson.Safe.write_int
            ) ob x
            );
            Bi_outbuf.add_char ob ')';
        ) ob x;
        Bi_outbuf.add_char ob '>'
)
let string_of_player ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_player ob x;
  Bi_outbuf.contents ob
let read_player = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 2 -> (
                      if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'i' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 5 -> (
                      if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'n' then (
                        0
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  Ag_oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Human x
            | 1 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            Ag_oj_run.read_string
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            Ag_oj_run.read_int
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `AI x
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 2 -> (
                      if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'i' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 5 -> (
                      if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'n' then (
                        0
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Ag_oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Human x
            | 1 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  fun p lb ->
                    Yojson.Safe.read_space p lb;
                    let std_tuple = Yojson.Safe.start_any_tuple p lb in
                    let len = ref 0 in
                    let end_of_tuple = ref false in
                    (try
                      let x0 =
                        let x =
                          (
                            Ag_oj_run.read_string
                          ) p lb
                        in
                        incr len;
                        Yojson.Safe.read_space p lb;
                        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        x
                      in
                      let x1 =
                        let x =
                          (
                            Ag_oj_run.read_int
                          ) p lb
                        in
                        incr len;
                        (try
                          Yojson.Safe.read_space p lb;
                          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                        with Yojson.End_of_tuple -> end_of_tuple := true);
                        x
                      in
                      if not !end_of_tuple then (
                        try
                          while true do
                            Yojson.Safe.skip_json p lb;
                            Yojson.Safe.read_space p lb;
                            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                          done
                        with Yojson.End_of_tuple -> ()
                      );
                      (x0, x1)
                    with Yojson.End_of_tuple ->
                      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `AI x
            | _ -> (
                assert false
              )
        )
)
let player_of_string s =
  read_player (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__5 = (
  Ag_oj_run.write_list (
    fun ob x ->
      Bi_outbuf.add_char ob '(';
      (let x, _ = x in
      (
        write_player
      ) ob x
      );
      Bi_outbuf.add_char ob ',';
      (let _, x = x in
      (
        Yojson.Safe.write_int
      ) ob x
      );
      Bi_outbuf.add_char ob ')';
  )
)
let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob
let read__5 = (
  Ag_oj_run.read_list (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      let std_tuple = Yojson.Safe.start_any_tuple p lb in
      let len = ref 0 in
      let end_of_tuple = ref false in
      (try
        let x0 =
          let x =
            (
              read_player
            ) p lb
          in
          incr len;
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          x
        in
        let x1 =
          let x =
            (
              Ag_oj_run.read_int
            ) p lb
          in
          incr len;
          (try
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          with Yojson.End_of_tuple -> end_of_tuple := true);
          x
        in
        if not !end_of_tuple then (
          try
            while true do
              Yojson.Safe.skip_json p lb;
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_tuple_sep2 p std_tuple lb;
            done
          with Yojson.End_of_tuple -> ()
        );
        (x0, x1)
      with Yojson.End_of_tuple ->
        Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
  )
)
let _5_of_string s =
  read__5 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_score_board = (
  write__5
)
let string_of_score_board ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_score_board ob x;
  Bi_outbuf.contents ob
let read_score_board = (
  read__5
)
let score_board_of_string s =
  read_score_board (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_letter : _ -> letter -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"character\":";
    (
      Yojson.Safe.write_string
    )
      ob x.character;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"pt\":";
    (
      Yojson.Safe.write_int
    )
      ob x.pt;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"count\":";
    (
      Yojson.Safe.write_int
    )
      ob x.count;
    Bi_outbuf.add_char ob '}';
)
let string_of_letter ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_letter ob x;
  Bi_outbuf.contents ob
let read_letter = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_character = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_pt = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_count = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 2 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 't' then (
                  1
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 't' then (
                  2
                )
                else (
                  -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'r' then (
                  0
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_character := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_pt := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_count := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 2 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 't' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 't' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'r' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_character := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_pt := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_count := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x7 then Ag_oj_run.missing_fields p [| !bits0 |] [| "character"; "pt"; "count" |];
        (
          {
            character = !field_character;
            pt = !field_pt;
            count = !field_count;
          }
         : letter)
      )
)
let letter_of_string s =
  read_letter (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__6 = (
  Ag_oj_run.write_list (
    write_letter
  )
)
let string_of__6 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__6 ob x;
  Bi_outbuf.contents ob
let read__6 = (
  Ag_oj_run.read_list (
    read_letter
  )
)
let _6_of_string s =
  read__6 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_player_rack = (
  fun ob x ->
    Bi_outbuf.add_char ob '(';
    (let x, _ = x in
    (
      write_player
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x = x in
    (
      fun ob x ->
        Bi_outbuf.add_char ob '(';
        (let x = x in
        (
          write__6
        ) ob x
        );
        Bi_outbuf.add_char ob ')';
    ) ob x
    );
    Bi_outbuf.add_char ob ')';
)
let string_of_player_rack ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_player_rack ob x;
  Bi_outbuf.contents ob
let read_player_rack = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            read_player
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            fun p lb ->
              Yojson.Safe.read_space p lb;
              let std_tuple = Yojson.Safe.start_any_tuple p lb in
              let len = ref 0 in
              let end_of_tuple = ref false in
              (try
                let x0 =
                  let x =
                    (
                      read__6
                    ) p lb
                  in
                  incr len;
                  (try
                    Yojson.Safe.read_space p lb;
                    Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                  with Yojson.End_of_tuple -> end_of_tuple := true);
                  x
                in
                if not !end_of_tuple then (
                  try
                    while true do
                      Yojson.Safe.skip_json p lb;
                      Yojson.Safe.read_space p lb;
                      Yojson.Safe.read_tuple_sep2 p std_tuple lb;
                    done
                  with Yojson.End_of_tuple -> ()
                );
                (x0)
              with Yojson.End_of_tuple ->
                Ag_oj_run.missing_tuple_fields p !len [ 0 ]);
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1)
    with Yojson.End_of_tuple ->
      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
)
let player_rack_of_string s =
  read_player_rack (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_direction = (
  fun ob sum ->
    match sum with
      | `Across -> Bi_outbuf.add_string ob "<\"across\">"
      | `Down -> Bi_outbuf.add_string ob "<\"down\">"
)
let string_of_direction ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_direction ob x;
  Bi_outbuf.contents ob
let read_direction = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 4 -> (
                      if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'w' && String.unsafe_get s (pos+3) = 'n' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 6 -> (
                      if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 's' then (
                        0
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Across
            | 1 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Down
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 4 -> (
                      if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'w' && String.unsafe_get s (pos+3) = 'n' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 6 -> (
                      if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 's' then (
                        0
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              `Across
            | 1 ->
              `Down
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | _ -> (
                assert false
              )
        )
)
let direction_of_string s =
  read_direction (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_coordinate = (
  fun ob x ->
    Bi_outbuf.add_char ob '(';
    (let x, _ = x in
    (
      Yojson.Safe.write_string
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x = x in
    (
      Yojson.Safe.write_int
    ) ob x
    );
    Bi_outbuf.add_char ob ')';
)
let string_of_coordinate ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_coordinate ob x;
  Bi_outbuf.contents ob
let read_coordinate = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            Ag_oj_run.read_string
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            Ag_oj_run.read_int
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1)
    with Yojson.End_of_tuple ->
      Ag_oj_run.missing_tuple_fields p !len [ 0; 1 ]);
)
let coordinate_of_string s =
  read_coordinate (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_play : _ -> play -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"word\":";
    (
      Yojson.Safe.write_string
    )
      ob x.word;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"direction\":";
    (
      write_direction
    )
      ob x.direction;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"coordinate\":";
    (
      write_coordinate
    )
      ob x.coordinate;
    Bi_outbuf.add_char ob '}';
)
let string_of_play ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_play ob x;
  Bi_outbuf.contents ob
let read_play = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_word = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_direction = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_coordinate = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'd' then (
                  0
                )
                else (
                  -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' then (
                  1
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'e' then (
                  2
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_word := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_direction := (
              (
                read_direction
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_coordinate := (
              (
                read_coordinate
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'd' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'e' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_word := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_direction := (
                (
                  read_direction
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_coordinate := (
                (
                  read_coordinate
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x7 then Ag_oj_run.missing_fields p [| !bits0 |] [| "word"; "direction"; "coordinate" |];
        (
          {
            word = !field_word;
            direction = !field_direction;
            coordinate = !field_coordinate;
          }
         : play)
      )
)
let play_of_string s =
  read_play (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  Ag_oj_run.write_list (
    Yojson.Safe.write_string
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  Ag_oj_run.read_list (
    Ag_oj_run.read_string
  )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_move = (
  fun ob sum ->
    match sum with
      | `Play x ->
        Bi_outbuf.add_string ob "<\"play\":";
        (
          write_play
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | `SwitchAll -> Bi_outbuf.add_string ob "<\"switchall\">"
      | `SwitchSome x ->
        Bi_outbuf.add_string ob "<\"switchsome\":";
        (
          write__1
        ) ob x;
        Bi_outbuf.add_char ob '>'
      | `Pass -> Bi_outbuf.add_string ob "<\"pass\">"
      | `Shuffle -> Bi_outbuf.add_string ob "<\"shuffle\">"
      | `End -> Bi_outbuf.add_string ob "<\"end\">"
)
let string_of_move ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_move ob x;
  Bi_outbuf.contents ob
let read_move = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 3 -> (
                      if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' then (
                        5
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 4 -> (
                      if String.unsafe_get s pos = 'p' then (
                        match String.unsafe_get s (pos+1) with
                          | 'a' -> (
                              if String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 's' then (
                                3
                              )
                              else (
                                raise (Exit)
                              )
                            )
                          | 'l' -> (
                              if String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'y' then (
                                0
                              )
                              else (
                                raise (Exit)
                              )
                            )
                          | _ -> (
                              raise (Exit)
                            )
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 7 -> (
                      if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'f' && String.unsafe_get s (pos+4) = 'f' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 'e' then (
                        4
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 9 -> (
                      if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'w' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 'l' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 10 -> (
                      if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'w' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = 'e' then (
                        2
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read_play
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Play x
            | 1 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `SwitchAll
            | 2 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read__1
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `SwitchSome x
            | 3 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Pass
            | 4 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `Shuffle
            | 5 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `End
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 3 -> (
                      if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'd' then (
                        3
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 4 -> (
                      if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 's' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 7 -> (
                      if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'h' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'f' && String.unsafe_get s (pos+4) = 'f' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 'e' then (
                        2
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 9 -> (
                      if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'w' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 'l' then (
                        0
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              `SwitchAll
            | 1 ->
              `Pass
            | 2 ->
              `Shuffle
            | 3 ->
              `End
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                match len with
                  | 4 -> (
                      if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'y' then (
                        0
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | 10 -> (
                      if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'w' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'h' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = 'e' then (
                        1
                      )
                      else (
                        raise (Exit)
                      )
                    )
                  | _ -> (
                      raise (Exit)
                    )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_play
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `Play x
            | 1 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__1
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              `SwitchSome x
            | _ -> (
                assert false
              )
        )
)
let move_of_string s =
  read_move (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_letter_bag = (
  write__6
)
let string_of_letter_bag ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_letter_bag ob x;
  Bi_outbuf.contents ob
let read_letter_bag = (
  read__6
)
let letter_bag_of_string s =
  read_letter_bag (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__7 = (
  Ag_oj_run.write_list (
    write_player_rack
  )
)
let string_of__7 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__7 ob x;
  Bi_outbuf.contents ob
let read__7 = (
  Ag_oj_run.read_list (
    read_player_rack
  )
)
let _7_of_string s =
  read__7 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_game_state : _ -> game_state -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"board\":";
    (
      write_scrabble_board
    )
      ob x.board;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"score_board\":";
    (
      write_score_board
    )
      ob x.score_board;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"letter_bag\":";
    (
      write_letter_bag
    )
      ob x.letter_bag;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"player_racks\":";
    (
      write__7
    )
      ob x.player_racks;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"turn\":";
    (
      Yojson.Safe.write_int
    )
      ob x.turn;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"counter\":";
    (
      Yojson.Safe.write_int
    )
      ob x.counter;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"quit\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.quit;
    Bi_outbuf.add_char ob '}';
)
let string_of_game_state ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_game_state ob x;
  Bi_outbuf.contents ob
let read_game_state = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_board = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_score_board = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_letter_bag = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_player_racks = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_turn = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_counter = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_quit = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                match String.unsafe_get s pos with
                  | 'q' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' then (
                        6
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'n' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'd' then (
                  0
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'r' then (
                  5
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'b' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'g' then (
                  2
                )
                else (
                  -1
                )
              )
            | 11 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'b' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'd' then (
                  1
                )
                else (
                  -1
                )
              )
            | 12 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'y' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'c' && String.unsafe_get s (pos+10) = 'k' && String.unsafe_get s (pos+11) = 's' then (
                  3
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_board := (
              (
                read_scrabble_board
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_score_board := (
              (
                read_score_board
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_letter_bag := (
              (
                read_letter_bag
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            field_player_racks := (
              (
                read__7
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | 4 ->
            field_turn := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x10;
          | 5 ->
            field_counter := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x20;
          | 6 ->
            field_quit := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x40;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  match String.unsafe_get s pos with
                    | 'q' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' then (
                          6
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'n' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'd' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'r' then (
                    5
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'b' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'g' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 11 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'b' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'd' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 12 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'y' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'a' && String.unsafe_get s (pos+9) = 'c' && String.unsafe_get s (pos+10) = 'k' && String.unsafe_get s (pos+11) = 's' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_board := (
                (
                  read_scrabble_board
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_score_board := (
                (
                  read_score_board
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_letter_bag := (
                (
                  read_letter_bag
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              field_player_racks := (
                (
                  read__7
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | 4 ->
              field_turn := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x10;
            | 5 ->
              field_counter := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x20;
            | 6 ->
              field_quit := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x40;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x7f then Ag_oj_run.missing_fields p [| !bits0 |] [| "board"; "score_board"; "letter_bag"; "player_racks"; "turn"; "counter"; "quit" |];
        (
          {
            board = !field_board;
            score_board = !field_score_board;
            letter_bag = !field_letter_bag;
            player_racks = !field_player_racks;
            turn = !field_turn;
            counter = !field_counter;
            quit = !field_quit;
          }
         : game_state)
      )
)
let game_state_of_string s =
  read_game_state (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
