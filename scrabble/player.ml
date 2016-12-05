open Utils
open Data
open Filter
open State

module type Player = sig
	(* type for input type *)
	type t

	(* [execute_move] is the new state based on user input and the move in
	 * in progress:
	 * [t] : input type. It varies by the type of player *)
	val execute_move : t -> Data.game_state -> Data.game_state

end

module Human : (Player with type t = string) = struct
	(* type for input type *)
	type t = string

	(* [check_coordinate] is a bool indicating the validity of input coordinate
	 * false means the invalid input for the coordinate, true means the coordinate
	 * is valid *)
	let check_coordinate (x,y) =
		let lower_x = String.lowercase_ascii x in
		 (1 <= y) && (y <= 15) &&
		 (Char.code lower_x.[0] >= Char.code 'a') &&
		 (Char.code 'o' >= (Char.code lower_x.[0]))

	(* [string_to_direction] is a type direction representation
	 * of string type input [s] *)
	let string_to_direction s =
	  match s with
	  | "across"-> Across
	  | "down" -> Down
	  | _ -> failwith "Invaild direction"

	(* [execute_move] is the updated state based on string type user input.
	 * parse the string input and varify the move, send the move type to
	 * filter module to check the validy of the move. only checks the
	 * input commands are invlaid, does not check the validy of the move in terms
	 * of game rules.
	 * [play] -> place a word on the board
	 * require: info for command, word, direction, and coordinate
	 * [swapall] -> swap all letters in the hand.
	 * [swapsome] -> swap some of letters in the hand.
	 * [pass] -> pass turn
	 * [shuffle] -> shuffle rack
	 * [quit] -> quit game
	 * raise excpetion Invalid if there is any ivlaid command
	 * Invalid command will not be sent to Filter but caught by GUI and displays
	 * current state, and error message "Invalid command" *)
	let execute_move s_move c_state =
		let split = Str.split (Str.regexp " +") (s_move ^ " ") in
		let move = String.lowercase_ascii (get_nth (split,0)) in
		let n = List.length split in
		let command = match move with
		| "play" | "p" ->
			let coordinate = ((get_nth (split, 3)),
			(int_of_string (get_nth (split, 4)))) in
			if n = 5 && (check_coordinate coordinate) then
			Play
			{
				word = get_nth (split, 1);
				direction = string_to_direction (String.lowercase_ascii
					(get_nth (split, 2)));
				coordinate = coordinate
			}
			else raise Invalid
		| "swapall" | "sa" ->
			if n = 1 then SwitchAll else raise Invalid
		| "swapsome" | "ss"->
			if n >= 2 then
				let char_lst = List.fold_left
					(fun acc x -> acc @ (string_to_char_list x)) []
					(List.tl split) in
				SwitchSome char_lst
			else raise Invalid
		| "pass" -> if n = 1 then Pass else raise Invalid
		| "shuffle" | "s" -> if n = 1 then Shuffle else raise Invalid
		| "quit" | "q" -> if n = 1 then End else raise Invalid
		| _ -> raise Invalid in
		validate command c_state

end


module AI : (Player with type t = Data.game_state) =  struct

	(* type for game state *)
	type t = Data.game_state

	(*type d = Data.direction*)

	type compass = North | South | East | West
	(* [check_tile_board] is the coordinate of first tile found on board from the
	 * right to left and then from top to bottom *)
	let rec check_tile_board board (x, y) =
		match (get_tile (x,y) board).letter with
			| None -> if x = 14 && y = 14 then ("z",(15,15)) else
				if x = 14 then check_tile_board board (0, (y+1))
					else check_tile_board board ((x+1), y)
			| Some c -> (c, (x,y))

	(*[space_check] is the number of empty tiles in a given [direction]
	on the tile with coordinate [coordinate] and scrabble board [board]*)
	let rec space_check (x, y) board direction =
		let (n_x, n_y) = match direction with
			| North -> (x - 1, y)
			| South -> (x + 1, y)
			| East  -> (x, y + 1)
			| West  -> (x, y - 1) in
		if (n_x < 0 || n_y < 0 || n_x > 14 || n_y > 14) then 0 else
			let tile = get_tile (n_x, n_y) board in
			if (tile.letter = None) then
			1 + space_check (n_x, n_y) board direction else 0

	(*[space_value] is a sequence of 4 ints (north, south, east, west)
  that correspond with the number of empty tiles in that direction.
  [Level] is an int that caps the number*)
	let space_value level coordinate board =
		let helper v = if v > level then level else v in
		let north = helper (space_check coordinate board North)  in
		let south = helper (space_check coordinate board South)  in
		let east  = helper (space_check coordinate board East)  in
		let west  = helper (space_check coordinate board West)   in
		 (north, south, east, west)

	(*[sublist] returns a list of the first [length] elements of the_list]*)
	let rec sublist the_list length =
		match the_list with
		| []   -> []
		| h::t -> if length > 0 then h :: sublist t (length - 1) else []

	(*[get_rack_letters] is a letter list with coordinate and (length - 1)
		 letters from the players rack*)
	let get_rack_letters rack length =
	 let (player, rack_letters) = rack in
		sublist (shuffle rack_letters) length

	(* [make_str] is a string list of single characters from a list of letters*)
	let rec make_str letterlst = match letterlst with
		| [] -> ""
		| hd::tl -> (hd.character) ^ (make_str tl)

	(*[permute] is a list of all the permutations of the chracters in
  [letterlst]*)
  let rec permute letterlst =
		match letterlst with
		| [] -> []
		| hd::[] -> [[hd]]
		| _ ->
			let helper acc v =
				acc @ List.map (fun l -> v::l) (permute (remove v letterlst)) in
			List.fold_left helper [] letterlst

	(*[str_permute] is a string list of all the permutations of [letterlst] sorted
  and without any duplicates *)
  let str_permute letterlst = List.sort_uniq (compare)
  (List.map make_str (permute letterlst))

	(*[prepend] is a string list with a length [length] string prepended onto
  character [ch] from a player's [rack] *)
	let prepend ch length rack =
		let letters = get_rack_letters rack length in
		let prems = str_permute letters in
		List.map (fun v -> v ^ ch) prems

  (*[append] is a string list with a length [length] string appended onto
  character [ch] from a player's [rack] *)
	let append ch length rack =
		let letters = get_rack_letters rack length in
		let prems = str_permute letters in
		List.map (fun v -> ch ^ v) prems

	(* let is_valid m s = true *)


  (*[char_to_int_brd] is the int corresponding to char [c] in a zero-indexed
  board*)
	let char_to_int_brd c = match c with
	| "a" -> 0 | "b" -> 1 | "c" -> 2 | "d" -> 3 | "e" -> 4 | "f" -> 5 | "g" -> 6
	| "h" -> 7 | "i" -> 8 | "j" -> 9 | "k" -> 10 | "l" -> 11 | "m" -> 12
	| "n" -> 13 | "o" -> 14 | _ -> failwith "out of bounds"

  (*[int_to_char_brd] is the char corresponding to int [c] in a zero-indexed
  board*)
	let int_to_char_brd c = match c with
	| 0 -> "a" | 1 -> "b" | 2 -> "c" | 3 -> "d" | 4 -> "e" | 5 -> "f" | 6 -> "g"
	| 7 -> "h" | 8 -> "i" | 9 -> "j" | 10 -> "k" | 11 -> "l" | 12 -> "m"
	| 13 -> "n" | 14 -> "o" | _ -> failwith "out of bounds"

	(*[ai_valid] is a boolean of the validity of move [v] in state [state]*)
  let ai_valid v state = try is_valid v state with _ -> false

	(*[make_move] is the first valid move played in direction [dir] from a list
  of valid moves generated from state [st]*)
  let rec make_move w_lst dir st coord length rack c: Data.move =
		let (n_dir, n_c) =
		 match dir with
			| North -> (Down, (fst coord - length, snd coord) )
			| South -> (Down, coord)
			| East -> (Across, coord)
			| West -> (Across, (fst coord, snd coord - length)) in
		let helper v =
			let (x1,y1) = n_c in
			Play{word = v;
			direction = n_dir; coordinate = (int_to_char_brd y1, (x1+1))} in
		let n_lst = List.map helper w_lst in
		match List.filter (fun x -> ai_valid x st) n_lst with
			| [] -> if length = 1 then
				check_moves st.board st (fst coord + 1, snd coord) else
				let l = length - 1 in
				let (w_l, d) =
					(match dir with
					| North -> ((prepend c l rack), North)
					| South -> ((append c l rack), South)
					| East -> ((append c l rack), East)
					| West -> ((prepend c l rack), West) ) in
				make_move w_l d st coord l rack c
			(* | hd::tl -> print_play hd; hd *)
			| hd::tl -> hd
	and check_moves board sta (x,y) =
		let rack = current_player_rack sta in
		let level =
			match fst rack with
			| AI (name, l) -> l
			| _ -> failwith "never happens" in
		let (ch, co) = check_tile_board board (x, y) in
		let (no, so, ea, we) = space_value level co board in
		if ((no = 0 || so = 0) && (ea = 0 || we = 0)) then
			(check_moves board sta (fst co + 1, snd co))
		else
			let rack = current_player_rack sta in
			let (wo_lst, dire, lth) = if no = 0 || so = 0 then
					(if (ea >= we) then ((append ch ea rack), East, ea)
					else ((prepend ch we rack), West, we) )
				else
					(if (no > so) then ((prepend ch no rack), North, no)
					else ((append ch so rack), South, so) )in
			make_move wo_lst dire sta co lth rack ch

  (*[empty_move] is a valid move given a state [c_state] when the scrabble board
  is empty*)
  let empty_move c_state =
    (* let board = c_state.board in *)
    let rack = current_player_rack c_state in
    let rack_letters = get_rack_letters rack 4 in
    let perm_list = str_permute rack_letters in
    let helper v =
    let (x1,y1) = ("h" , 8) in
      Play { word   = v;
             direction  = Across;
            coordinate = (x1,y1) }
     in let move_list = List.map helper perm_list in
     let valid_list = List.filter
      (* (fun x -> print_play x; is_valid x c_state) move_list in *)
			(fun x -> try is_valid x c_state with _ -> false ) move_list in
       try List.hd valid_list with |_ -> SwitchAll

	(*[execute_move] is the new state after a valid move is played by the AI
  given an initial state [s_state]*)
  let execute_move s_state c_state =
		let board = c_state.board in
    let move = if ((check_tile_board board (0,0)) = ("z",(15,15))) then
      empty_move c_state
    else
	    try check_moves board c_state (0,0) with
	    | _ -> let hand = [] in
			match hand with
			| [] ->  SwitchAll
			| _ -> failwith "no" in

			(* let n = List.length hand in
			let sorted_hand = List.rev
			(List.sort (fun x y -> Pervasives.compare x.pt y.pt) hand) in
			let ones = List.filter (fun x -> x.character <> "E" || x.character <> "A")
			sorted_hand in
			let rec split n lst =
				if n = 0 then []
				else get_nth (lst, n-1) :: split (n-1) lst in
			let high_char = letter_to_char (split (n/2) sorted_hand) in
			match ones with *)
			(* | [] -> SwitchAll
			| _ -> SwitchSome (letter_to_char ones) in *)
		validate move s_state

end
