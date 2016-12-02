open Utils
open Data
open Filter
open State
exception No_Tile_Found
exception Invalid

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
		let move = String.lowercase_ascii (List.nth split 0) in
		let n = List.length split in
		let command = match move with
		| "play" | "p" ->
			let coordinate = (String.get (List.nth split 3) 0,
			(int_of_string (List.nth split 4))) in
			if n = 5 && (check_coordinate coordinate) then
			Play
			{
				word = List.nth split 1;
				direction = string_to_direction (String.lowercase_ascii (List.nth split 2));
				coordinate = coordinate
			}
			else raise Invalid
		| "swapall" | "sa" ->
			if n = 1 then SwitchAll else raise Invalid
		| "swapsome" | "ss"->
			if n >= 2 then
				let char_lst = List.map
				(fun x -> if String.length x = 1 then String.get x 0 else raise Invalid)
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
			| None -> if x = 14 && y = 14 then ('z',(15,15)) else
				if x = 14 then check_tile_board board (0, (y+1))
					else check_tile_board board ((x+1), y)
			| Some c ->
				(* let () = print_int x; print_string " * "; print_int (y); print_endline "= coordinate found" in *)
				(c, (x,y))

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

	(**)
	let space_value coordinate board =
		let helper v = if v > 3 then 3 else v in
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
		sublist rack_letters length

	(* persmutaion of all the strings*)
	let rec make_str letterlst = match letterlst with
		| [] -> ""
		| hd::tl -> (String.make 1 hd.character) ^ (make_str tl)

  let rec remove v lst =
  match lst with
  | [] -> []
  | h::t -> if v = h then t else h :: remove v t

	let rec permute letterlst =
		match letterlst with
		| [] -> []
		| hd::[] -> [[hd]]
		| _ ->
			let helper acc v =
				acc @ List.map (fun l -> v::l) (permute (remove v letterlst)) in
			List.fold_left helper [] letterlst

	let str_permute letterlst =List.sort_uniq (compare) (List.map make_str (permute letterlst))

	(*make a move check if its valid*)
	let prepend ch length rack =
		let letters = get_rack_letters rack length in
		let prems = str_permute letters in
		List.map (fun v -> v ^ (String.make 1 ch)) prems

	let append ch length rack =
		let letters = get_rack_letters rack length in
		let prems = str_permute letters in
		List.map (fun v -> (String.make 1 ch) ^ v) prems

	(* let is_valid m s = true *)

	let char_to_int_brd c = match c with
	| 'a' -> 0 | 'b' -> 1 | 'c' -> 2 | 'd' -> 3 | 'e' -> 4 | 'f' -> 5 | 'g' -> 6
	| 'h' -> 7 | 'i' -> 8 | 'j' -> 9 | 'k' -> 10 | 'l' -> 11 | 'm' -> 12
	| 'n' -> 13 | 'o' -> 14 | _ -> failwith "out of bounds"

	let int_to_char_brd c = match c with
	| 0 -> 'a' | 1 -> 'b' | 2 -> 'c' | 3 -> 'd' | 4 -> 'e' | 5 -> 'f' | 6 -> 'g'
	| 7 -> 'h' | 8 -> 'i' | 9 -> 'j' | 10 -> 'k' | 11 -> 'l' | 12 -> 'm'
	| 13 -> 'n' | 14 -> 'o' | _ -> failwith "out of bounds"

	let print_play p = match p with
	  | Play {
	      word = str;
	      direction = dir;
	      coordinate = crd } ->
	        let () = print_string (str ^ " was played by AI at ") in
	        let () = print_char (fst crd); print_string " * "; print_int (snd crd); in
	        if dir = Across then print_endline " across" else print_endline " down"
	  | _ -> failwith "something bad"

	let rec make_move w_lst dir st coord length : Data.move =
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
		(* match List.filter (fun x -> print_play x; is_valid x st) n_lst with *)
		match List.filter (fun x -> try is_valid x st with _ -> false) n_lst with
			| [] -> check_moves st.board st (fst coord + 1, snd coord)
			| hd::tl -> hd

	and check_moves board sta (x,y) =
		let (ch, co) = check_tile_board board (x, y) in
		let (no, so, ea, we) = space_value co board in
		if ((no = 0 || so = 0) && (ea = 0 || we = 0)) then
			(check_moves board sta (fst co + 1, snd co))
		else
			let rack = current_player_rack sta in
			let (wo_lst, dire, lth) = if no = 0 || so = 0 then
					(if (ea >= we) then ((append ch ea rack), East, ea)
					else ((prepend ch we rack), West, we) )
				else
					(if (no >= so) then ((prepend ch no rack), North, no)
					else ((append ch so rack), South, so) )in
			make_move wo_lst dire sta co lth

  let empty_move c_state =
    (* let board = c_state.board in *)
    let rack = current_player_rack c_state in
    let rack_letters = get_rack_letters rack 4 in
    let perm_list = str_permute rack_letters in
    let helper v =
    let (x1,y1) = ('h' , 8) in
      Play { word   = v;
             direction  = Across;
            coordinate = (x1,y1) }
     in let move_list = List.map helper perm_list in
     let valid_list = List.filter
      (* (fun x -> print_play x; is_valid x c_state) move_list in *)
			(fun x -> try is_valid x c_state with _ -> false ) move_list in
       try List.hd valid_list with |_ -> SwitchAll

	let execute_move s_state c_state =
		let board = c_state.board in
    let move = if ((check_tile_board board (0,0)) = ('z',(15,15))) then
      empty_move c_state
    else
    try check_moves board c_state (0,0) with
    | _ -> SwitchAll in
		validate move s_state

end
