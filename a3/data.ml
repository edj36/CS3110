exception Unimplemented

module type Comparable =
  sig
    type t
    val compare : t -> t -> [ `EQ | `GT | `LT ]
    val format : Format.formatter -> t -> unit
  end

module type Dictionary =
  sig
    module Key : Comparable
    type key = Key.t
    type 'value t
    val rep_ok : 'value t  -> 'value t
    val empty : 'value t
    val is_empty : 'value t -> bool
    val size : 'value t -> int
    val insert : key -> 'value -> 'value t -> 'value t
    val member : key -> 'value t -> bool
    val find : key -> 'value t -> 'value option
    val remove : key -> 'value t -> 'value t
    val choose : 'value t -> (key * 'value) option
    val fold : (key -> 'value -> 'acc -> 'acc) -> 'acc -> 'value t -> 'acc
    val to_list : 'value t -> (key * 'value) list
    val format : (Format.formatter -> 'value -> unit)
                  -> Format.formatter -> 'value t -> unit
  end

module type DictionaryMaker =
  functor (C : Comparable) -> Dictionary with type Key.t = C.t

module MakeListDictionary (C : Comparable) = struct
  module Key = C
  type key = C.t

  (* TODO: getting type ['value t] to something involving
     association lists. *)
  (* AF: TODO: document the abstraction function.
   * RI: TODO: document any representation invariants. *)
  type 'value t = (key * 'value) list

  let comparing (k1,v1) (k2,v2) = match C.compare k1 k2 with
    |`EQ -> 0
    |`GT -> 1
    |`LT -> -1

  let rep_ok d =
    if ((List.length d) = (List.length (List.sort_uniq comparing d))) then d
    else raise (Failure "Representation not ok")

  (*let rep_ok d =
    if not (List.fold_left (fun acc (x1,y1) -> (List.length (List.filter (fun (x2,y2) ->
    (C.compare x1 x2)=`EQ) d)) >= 2 || acc) false d) then d else raise Failure*)

  let empty = []

  let is_empty d = (d=empty)

  let size d = List.length d

  let insert k v d =
    (k,v)::(List.filter (fun (x,y) -> if (C.compare x k)=`EQ then false else true) d)

  let remove k d =
    List.filter (fun (x,y) -> if (C.compare x k)=`EQ then false else true) d

  let find k d =
    List.fold_left (fun acc (x,y) -> if (C.compare x k)=`EQ then Some y else acc) None d

  let member k d =
    List.fold_left (fun acc (x,y) -> (C.compare x k)=`EQ || acc) false d

  let choose d = match d with
  | (k,v)::t -> Some (k,v)
  | [] -> None

  let to_list d = List.sort comparing d

  let fold f init d =
    List.fold_left (fun acc (x,y) -> f x y acc) init (to_list d)

  let format format_val fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)

end

(* TODO (later): implement a 2-3 tree.  For now,
   this code punts by equating the two functors. *)
module MakeTreeDictionary (C : Comparable) = struct
  module Key = C
  type key = C.t

  (* TODO: getting type ['value t] to something involving
     association lists. *)
  (* AF: TODO: document the abstraction function.
   * RI: TODO: document any representation invariants. *)
  type 'value t =
    | Leaf
    | Two_Node of 'value t * (key * 'value) * 'value t
    | Three_Node of 'value t * (key * 'value)
      * 'value t * (key * 'value) * 'value t


  let rep_ok d =
    raise Unimplemented

  let empty = Leaf

  let is_empty d = match d with
  | Leaf -> true
  | _ -> false

  let rec size d = match d with
  | Leaf -> 0
  | Two_Node (w, (x,y), z) -> 1 + size w + size z
  | Three_Node (t, (u,v), w, (x,y), z) -> 2 + size t + size w + size z

  let remove k d =
    raise Unimplemented

  let rec insert k v d =

    raise Unimplemented
  (*match d with
  | Leaf -> (Leaf, (k,v), Leaf)
  | Two_Node (x, (k1,v1), y) -> if (C.compare k1 k = `EQ) then (x, (k,v), y) else
    if (C.compare k k1 = `LT) then (insert k v x)*)

  let rec find k d = match d with
  | Leaf -> None
  | Two_Node (w, (x,y), z) -> if (C.compare k x = `EQ) then Some y else if
    (C.compare k x = `LT) then find k w else find k z
  | Three_Node (t, (u,v), w, (x,y), z) -> if (C.compare k u = `EQ) then Some v else if
    (C.compare k u = `LT) then find k t else if (C.compare k x =`EQ) then Some y else if
    (C.compare k x = `LT) then find k w else find k z

  let member k d = find k d <> None

  let choose d = match d with
  | Leaf -> None
  | Two_Node (w, (x,y), z) -> Some (x,y)
  | Three_Node (t, (u,v), w, (x,y), z) -> Some (u,v)

  let rec to_list d = match d with
  | Leaf -> []
  | Two_Node (w, (x,y), z) -> (to_list w) @ (x,y)::(to_list z)
  | Three_Node (t, (u,v), w, (x,y), z) -> (to_list t) @ (u,v)::(to_list w) @ (x,y)::(to_list z)


  let fold f init d =
    List.fold_left (fun acc (x,y) -> f x y acc) init (to_list d)

  let format format_val fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)

end

module type Set =
  sig
    module Elt : Comparable
    type elt = Elt.t
    type t
    val rep_ok : t  -> t
    val empty : t
    val is_empty : t -> bool
    val size : t -> int
    val insert : elt -> t -> t
    val member : elt -> t -> bool
    val remove : elt -> t -> t
    val union : t -> t -> t
    val intersect : t -> t -> t
    val difference : t -> t -> t
    val choose : t -> elt option
    val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
    val to_list : t -> elt list
    val format : Format.formatter -> t -> unit
  end

module type SetMaker =
  functor (C : Comparable) -> Set with type Elt.t = C.t

(* HINT:  To build a set out of a dictionary, consider this:
   a dictionary is much like a **set** of (key,value) pairs. *)
module MakeSetOfDictionary (D:Dictionary) = struct
  module Elt = D.Key
  type elt = Elt.t

  (* TODO: change type [t] to something involving a dictionary *)
  type t = unit D.t

  let rep_ok s = raise Unimplemented

  let empty = D.empty

  let is_empty s = D.is_empty s

  let size s = D.size s

  let insert x s = D.insert x () s

  let member x s = D.member x s

  let remove x s = D.remove x s

  let choose s = match D.choose s with
  | Some (x,()) -> Some x
  | None -> None

  let fold f init s = D.fold (fun x y acc -> f x acc) init s

  let union s1 s2 = fold insert s1 s2

  let intersect s1 s2 = fold (fun elem acc -> if member elem s2 then insert elem acc else acc) empty s1

  let difference s1 s2 = fold remove s1 s2

  let to_list s = List.map (fun (x,y) -> x) (D.to_list s)

  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end


