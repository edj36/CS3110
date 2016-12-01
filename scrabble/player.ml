open Utils
open Data
open Filter
exception End

module type Player = sig

	(* type for game state *)
	type t

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
	val execute_move : t -> Data.game_state -> Data.game_state

end

module Human : (Player with type t = string) = struct

	(* type for game state *)
	type t = string

	(* [check_coordinate] is a bool indicating the validity of input coordinate
	 * false means the invalid input for the coodinate, true mweans the coodinate
	 * is valid *)
	let check_coordinate (x,y) =
		let lower_x = Char.lowercase_ascii x in
		 (1 <= y) && (y <= 15)
		 && (Char.code lower_x >= Char.code 'a')
		 && (Char.code 'o' >= Char.code lower_x)

	let execute_move s_move c_state =
		let split = Str.split (Str.regexp " +") (s_move ^ " ") in
		let move = List.nth split 0 in
		let n = List.length split in
		let command = match move with
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
				let char_lst = List.map (fun x -> String.get x 0) (List.tl split) in
				SwitchSome char_lst
			else failwith "Invalid command"
		| "Pass" | "pass" -> if n = 1 then Pass else failwith "Invalid command"
		| "Shuffle" | "shuffle" -> if n = 1 then Shuffle else failwith "Invalid command"
		| "End" | "end" -> if n = 1 then raise End else failwith "Invalid command"
		| _ -> failwith "Invalid command" in
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
			| None -> if x = 14 && y = 14 then failwith "Unimplemented" else 
				if x = 14 then check_tile_board board (0, (y+1)) 
					else check_tile_board board ((x+1), y)
			| Some c -> 
				let () = print_int x; print_string " * "; print_int (y); print_endline "= coordinate found" in
				(c, (x,y)) 

	(*[space_check] is the number of empty tiles in a given [direction]
	on the tile with coordinate [coordinate] and scrabble board [board]*)
	let rec space_check (x, y) board direction =
		let (n_x, n_y) = match direction with
			| North -> (x, y - 1)
			| South -> (x, y + 1)
			| East  -> (x + 1, y)
			| West  -> (x - 1, y) in
		if (n_x < 0 || n_y < 0 || n_x >= 15 || n_y >= 15) then 0 else
			let tile = get_tile (n_x, n_y) board in
			if (tile.letter = None) then 
			1 + space_check (n_x, n_y) board direction else 0

	(**)
	let space_value coordinate board =
		let helper v = if v > 4 then 4 else v in
		let north = helper (space_check coordinate board North)  in
		let south = helper (space_check coordinate board South)  in
		let east  = helper (space_check coordinate board East)  in
		let west  = helper (space_check coordinate board West)   in
		 (north, south, east, west)

	(*[sublist] returns a list of the first [length] elements of the_list]*)
	let rec sublist the_list length =
		match the_list with
		| []   -> []
		| h::t -> if length >= 0 then h :: sublist t (length - 1) else []

	(*[get_rack_letters] is a letter list with coordinate and (length - 1)
		 letters from the players rack*)
	let get_rack_letters rack length =
	 let (player, rack_letters) = rack in
		sublist rack_letters length

	(* persmutaion of all the strings*)
	let rec make_str letterlst = match letterlst with
		| [] -> ""
		| hd::tl -> (String.make 1 hd.character) ^ (make_str tl)

	let rec permute letterlst = 
		match letterlst with
		| [] -> []
		| hd::[] -> [[hd]]
		| _ -> 
			let remove v lst = List.filter (fun x -> not (x = v)) lst in 
			let helper acc v = 
				acc @ List.map (fun l -> v::l) (permute (remove v letterlst)) in 
			List.fold_left helper [] letterlst

	let str_permute letterlst =List.map make_str (permute letterlst)

	(*make a move check if its valid*)
	let prepend ch length rack = 
		let letters = get_rack_letters rack length in
		let prems = str_permute letters in
		List.map (fun v -> v ^ (String.make 1 ch)) prems

	let append ch length rack = 
		let letters = get_rack_letters rack length in
		let prems = str_permute letters in
		List.map (fun v -> (String.make 1 ch) ^ v) prems

	let is_valid m s = true

	let char_to_int_brd c = match c with 
	| 'a' -> 0 | 'b' -> 1 | 'c' -> 2 | 'd' -> 3 | 'e' -> 4 | 'f' -> 5 | 'g' -> 6
	| 'h' -> 7 | 'i' -> 8 | 'j' -> 9 | 'k' -> 10 | 'l' -> 11 | 'm' -> 12 
	| 'n' -> 13 | 'o' -> 14 | _ -> failwith "out of bounds"

	let int_to_char_brd c = match c with 
	| 0 -> 'a' | 1 -> 'b' | 2 -> 'c' | 3 -> 'd' | 4 -> 'e' | 5 -> 'f' | 6 -> 'g'
	| 7 -> 'h' | 8 -> 'i' | 9 -> 'j' | 10 -> 'k' | 11 -> 'l' | 12 -> 'm' 
	| 13 -> 'n' | 14 -> 'o' | _ -> failwith "out of bounds"

	let rec make_move w_lst dir st coord length : Data.move = 
		let (n_dir, n_c) = 
		 match dir with
			| North -> (Down, (fst coord, snd coord - (length)) )
			| South -> (Down, coord)
			| East -> (Across, (fst coord - (length), snd coord))
			| West -> (Across, coord) in
		let helper v = 
			let (x1,y1) = n_c in 
			Play{word = v; 
			direction = n_dir; coordinate = (int_to_char_brd x1, y1)} in 
		let n_lst = List.filter (fun x -> is_valid (helper x) st) w_lst in
		match List.map helper n_lst with 
			| [] -> check_moves st.board st (fst coord + 1, snd coord)
			| hd::tl -> 
				(* let () = print_endline (hd.word) in *)
				(* let () = print_int (fst hd.coordinate); print_string " * "; print_int (snd hd.coord); print_endline ""; in *)
				hd

	and check_moves board sta (x,y) = 
		let (ch, co) = check_tile_board board (x, y) in
		let (no, so, ea, we) = space_value co board in
		if ((no = 0 || so = 0) && (ea = 0 || we = 0)) then 
			(check_moves board sta (fst co + 1, snd co))
		else 
			let rack = current_player_rack sta in 
			let (wo_lst, dire, lth) = match (no,so) with
				| (0,0) -> if (ea >= we) then ((prepend ch ea rack), East, ea) 
					else ((append ch we rack), West, we) 
				| _     -> if (no >= so) then ((prepend ch no rack), North, no) 
					else ((append ch so rack), South, so) in 
			make_move wo_lst dire sta co lth

	let execute_move s_state c_state = 
		let board = c_state.board in
		let move = check_moves board c_state (0,0) in
		validate move s_state

end
