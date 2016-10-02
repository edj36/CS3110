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
  type 'value t = key * 'value list

  let comparing (k1,v1) (k2,v2) = match C.compare k1 k2 with
    |`EQ -> 0
    |`GT -> 1
    |`LT -> -1

  let rep_ok d =
    if ((List.length d) = (List.length (List.sort_uniq comparing d))) then d
    else raise Failure

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
    List.fold_left (fun acc (x,y) -> if (C.compare x k)=`EQ then y else acc) None d

  let member k d =
    List.fold_left (fun acc (x,y) -> (C.compare x k)=`EQ || acc) false d

  let choose d = match d with
  | (k,v)::t -> Some (k,v)
  | [] -> None

  let to_list d = List.sort comparing d

  let fold f init d =
    List.fold_left (fun (x,y) acc -> f x y acc) init (to_list d)

  let format format_val fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)

end

(* TODO (later): implement a 2-3 tree.  For now,
   this code punts by equating the two functors. *)
module MakeTreeDictionary = MakeListDictionary

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
  type t = unit

  let rep_ok s = raise Unimplemented

  let empty =
    raise Unimplemented

  let is_empty s =
    raise Unimplemented

  let size s =
    raise Unimplemented

  let insert x s =
    raise Unimplemented

  let member x s =
    raise Unimplemented

  let remove x s =
    raise Unimplemented

  let choose s =
    raise Unimplemented

  let fold f init s =
    raise Unimplemented

  let union s1 s2 =
    raise Unimplemented

  let intersect s1 s2 =
    raise Unimplemented

  let difference s1 s2 =
    raise Unimplemented

  let to_list s =
    raise Unimplemented

  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end


