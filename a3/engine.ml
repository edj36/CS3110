exception Unimplemented
exception Not_found

(* module that implements comparable for strings *)
module StringComparable = struct
  type t = string
  let compare x y = 
    if Pervasives.compare x y < 0 then `LT 
    else if Pervasives.compare x y = 0 then `EQ 
    else `GT
  let format something somethingelse = () 
end 

(* module that implements comparable for ints *)
module IntComparable = struct
  type t = int
  let compare x y = 
    if Pervasives.compare x y < 0 then `LT 
    else if Pervasives.compare x y = 0 then `EQ 
    else `GT
  let format something somethingelse = () 
end 

module type Engine = sig
  type idx
  val index_of_dir : string -> idx
  val to_list : idx -> (string * string list) list
  val or_not  : idx -> string list -> string list -> string list
  val and_not : idx -> string list -> string list -> string list
  val format : Format.formatter -> idx -> unit
end

module MakeEngine (S:Data.Set with type Elt.t = string)
  (D:Data.Dictionary with type Key.t = string) : Engine = struct

  type idx = S.t D.t 

  (* [index_of_line] is a new Dictionary with the [words] 
   * and corresponding files [f] (and directories [d]) inserted
   * into an existing dictionary [dict], for every word in [words] *)
  let rec index_of_line d f words dict = 
    match words with
    | [] -> dict 
    | h :: t -> 
      let s =  D.find h dict in match s with 
        | None -> 
          let new_s = S.insert (d ^ Filename.dir_sep ^ f) S.empty in 
          let new_dict = D.insert h new_s dict in 
            index_of_line d f t new_dict 
        | Some x -> 
          let new_s = S.insert (d ^ Filename.dir_sep ^ f) x in 
          let new_dict = D.insert h new_s dict in 
            index_of_line d f t new_dict 
        

  (* [file_helper] is a new Dictionary from [dict], adds 
   * the words and corresponding files for every word in every line [l]
   * in the file [f] within the directory [d] that is being 
   * processed to the new Dictionary and returns it *)
  let rec file_helper d f l dict = 
    try                                           
      let line = input_line l in                  (* get line to check *)
      let l_de = " " ^ line ^ " " in 
      let w = Str.split (Str.regexp "[^0-9a-zA-Z]*[ \t]+[^0-9a-zA-Z]*") l_de in
      let w_low = List.map (String.lowercase_ascii) w in  
        file_helper d f l (index_of_line d f w_low dict)  (* check next line *)
    with 
      | End_of_file -> dict                       (* base case *)
  
  (* [index_of_file] is a new Dictionary from [dict] with all of the 
   * words inside of the file [f] in the directory [d] added with ,
   * with their corresponding values *)
  let index_of_file d f dict = 
    let line = open_in (d ^ Filename.dir_sep ^ f) in
    let dict = file_helper d f line dict in  
    let _ = close_in line in                      (* close the file *)
      dict 

  (* [dir_helper] is a new Dictionary from [dict] with all of the 
   * words inside of files inside of the directory [dir] (Unix dir_handle)
   * directory is also represented by its string [d] for file paths later *)
  let rec dir_helper d dir dict = 
    try 
      match Unix.readdir dir with                 (* get file to check *)
      | n when Filename.check_suffix n "txt" ->   (* open .txt files *)
          dir_helper d dir (index_of_file d n dict)
      | _ -> dir_helper d dir dict                  (* check next file *)
    with 
      | End_of_file -> dict                       (* base case *)
  
  let index_of_dir d =
    try 
      let dir = Unix.opendir d in                 (* open directory *)
      let dict = dir_helper d dir D.empty in 
      let _ = Unix.closedir dir in                (* close directory *)
        dict
    with 
      | _ -> raise Not_found
    
  (* [to_list_snd] is S.to_list applies on the snd value in the
   * tuple (k,v) *)
  let to_list_snd (k,v) = match (k,v) with 
  | (k,v) -> (k, S.to_list v) 
    
  let to_list idx =
    let f = D.to_list idx  in 
    let s = List.map (to_list_snd) f in 
      s

  (* [f_set] is the set resulting from running [f] on Index [idx] and a 
   * list of [words], [acc_set] accumulates the sets for
   * recursion, and [f] is a function from the Set module, 
   * either union or intersection *)
  let rec f_set f idx words acc_set = match words with 
  | [] -> acc_set
  | h :: t -> let s = D.find h idx in match s with 
    | None -> f_set f idx t acc_set 
    | Some s -> let new_acc = f s acc_set in 
      f_set f idx t new_acc

  let or_not idx ors nots =
    let ors_l = List.map (String.lowercase_ascii) ors in 
    let nots_l = List.map (String.lowercase_ascii) nots in 
    let or_set = f_set S.union idx ors_l S.empty in 
    let not_set = f_set S.union idx nots_l S.empty in 
    let dif_set = S.difference or_set not_set in 
      S.to_list dif_set

  let and_not idx ands nots =
    let ands_l = List.map (String.lowercase_ascii) ands in 
    let nots_l = List.map (String.lowercase_ascii) nots in
    let first_acc = f_set S.union idx ands S.empty in 
    let and_set = f_set S.intersect idx ands first_acc in 
    let not_set = f_set S.union idx nots S.empty in 
    let dif_set = S.difference and_set not_set in 
      S.to_list dif_set


  let format fmt idx =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end

(* TODO: delete the entire struct below and replace it with
   a call to [MakeEngine] on some appropriate parameters. *)
module ListDict = Data.MakeListDictionary(StringComparable)
module ListSet = Data.MakeSetOfDictionary(ListDict)

module ListEngine = MakeEngine(ListSet)(ListDict)

(* TODO (later): after you've implemented 2-3 trees,
   replace this definition of [TreeEngine] with one
   that calls [MakeEngine] on some appropriate parameters.
   For now, this code punts by equating the two modules. *)
module TreeDict = Data.MakeTreeDictionary(StringComparable)
module TreeSet = Data.MakeSetOfDictionary(TreeDict)

module TreeEngine = MakeEngine(TreeSet)(TreeDict)
