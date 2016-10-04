exception Unimplemented
exception Unreachable

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

(* returns a merged tree of d1 and d2 and true if d3's height is less than that
 * of d2 *)
  let fix_tree d1 d2 = match d2 with
  | Two_Node (d1,(x,y),r) ->
    begin
      match r with
      | Two_Node (a,(sub1,val1),b) ->
        (Three_Node (d1,(x,y),a,(sub1,val1),b),true)
      | Three_Node (a, (sub1,val1), b, (sub2,val2), c) ->
        (Two_Node (Two_Node (d1,(x,y),a),(sub1,val1),
        Two_Node (b,(sub2,val2),c)),false)
      | Leaf -> raise Unreachable
    end
  | Two_Node (l,(x,y),d1) ->
    begin
      match l with
      | Two_Node (a,(sub1,val1),b) ->
        (Three_Node (a,(sub1,val1),b,(x,y),d1),true)
      | Three_Node (a, (sub1,val1), b, (sub2,val2), c) ->
        (Two_Node (Two_Node (a,(sub1,val1),b),(sub2,val2),
        Two_Node (c,(x,y),d1)),false)
      | Leaf -> raise Unreachable
    end
  | Three_Node (d1, (w,x), m, (y,z), r) ->
    begin
      match m with
      | Two_Node (a,(sub1,val1),b) ->
        (Two_Node (Three_Node (d1,(w,x),a,(sub1,val1),b),(y,z),r),false)
      | Three_Node (a, (sub1,val1), b, (sub2,val2), c) ->
        (Three_Node (Two_Node (d1,(w,x),a),(sub1,val1),
        Two_Node (b,(sub2,val2),c),(y,z),r),false)
      | Leaf -> raise Unreachable
    end
  | Three_Node (l, (w,x), d1, (y,z), r) ->
    begin
      match l with
      | Two_Node (a,(sub1,val1),b) ->
        (Two_Node (Three_Node (a,(sub1,val1),b,(w,x),d1),(y,z),r),false)
      | Three_Node (a, (sub1,val1), b, (sub2,val2), c) ->
        (Three_Node (Two_Node (a,(sub1,val1),b),(sub2,val2),
        Two_Node (c,(w,x),d1),(y,z),r),false)
      | Leaf -> raise Unreachable
    end
  | Three_Node (l, (w,x), m, (y,z), d1) ->
    begin
      match m with
      | Two_Node (a,(sub1,val1),b) ->
        (Two_Node (l,(w,x),Three_Node (a,(sub1,val1),b,(y,z),d1)),false)
      | Three_Node (a, (sub1,val1), b, (sub2,val2), c) ->
        (Three_Node (l,(w,x),Two_Node (a,(sub1,val1),b),(sub2,val2),
        Two_Node (c,(y,z),d1)),false)
      | Leaf -> raise Unreachable
    end
  | _ -> raise Unreachable

  let rec find_biggest d = match d with
  | Leaf -> raise Unreachable
  | Two_Node (Leaf,(x,y), Leaf) -> (x,y)
  | Three_Node (Leaf, (w,x), Leaf, (y,z), Leaf) -> (y,z)
  | Two_Node (l,(x,y),r) -> find_biggest r
  | Three_Node (l, (w,x), m, (y,z), r) -> find_biggest r

  let rec remove_helper k1 d = match d with
  | Leaf -> (Leaf,false)
  | Two_Node (Leaf, (x,y), Leaf) ->
    begin
      match (C.compare k1 x) with
      | `EQ -> (Leaf,true)
      | _ -> (d,false)
    end
  | Three_Node (Leaf, (w,x), Leaf, (y,z), Leaf) ->
    begin
      match (C.compare k1 w) with
      | `EQ -> (Two_Node (Leaf, (y,z), Leaf),false)
      | `LT -> (d,false)
      | `GT ->
        begin
          match (C.compare k1 y) with
          | `EQ -> (Two_Node (Leaf, (w,x), Leaf),false)
          | _ -> (d,false)
        end
    end
  | Two_Node (l,(x,y),r) ->
    begin
      match (C.compare k1 x) with
      | `EQ ->
        begin
          match l with
          | Two_Node (a,(sub1,val1),b) ->
            let left_biggest = find_biggest b in
            let (sub_tree, shrunk) = remove_helper (fst left_biggest) l in
            if (not shrunk) then (Two_Node (sub_tree,left_biggest,r),false)
            else fix_tree sub_tree (Two_Node (sub_tree,left_biggest,r))
          | Three_Node (a,(sub1,val1),b,(sub2,val2),c) ->
            let left_biggest = find_biggest c in
            let (sub_tree, shrunk) = remove_helper (fst left_biggest) l in
            if (not shrunk) then (Two_Node (sub_tree,left_biggest,r),false)
            else fix_tree sub_tree (Two_Node (sub_tree,left_biggest,r))
          | Leaf -> raise Unreachable
        end
      | `LT -> let (sub_tree, shrunk) = remove_helper k1 l in
        if (not shrunk) then (Two_Node (sub_tree,(x,y),r), false) else
        fix_tree sub_tree (Two_Node (sub_tree,(x,y),r))
      | `GT -> let (sub_tree, shrunk) = remove_helper k1 r in
        if (not shrunk) then (Two_Node (l,(x,y),sub_tree), false) else
        fix_tree sub_tree (Two_Node (l,(x,y),sub_tree))
    end
  | Three_Node (l, (w,x), m, (y,z), r) ->
    begin
      match (C.compare k1 w) with
      | `EQ ->
        begin
          match l with
          | Two_Node (a,(sub1,val1),b) ->
            let left_biggest = find_biggest b in
            let (sub_tree, shrunk) = remove_helper (fst left_biggest) l in
            if (not shrunk)
            then (Three_Node (sub_tree,left_biggest,m,(y,z),r),false)
            else fix_tree sub_tree (Three_Node (sub_tree,left_biggest,m,(y,z),r))
          | Three_Node (a,(sub1,val1),b,(sub2,val2),c) ->
            let left_biggest = find_biggest c in
            let (sub_tree, shrunk) = remove_helper (fst left_biggest) l in
            if (not shrunk)
            then (Three_Node (sub_tree,left_biggest,m,(y,z),r),false)
            else fix_tree sub_tree (Three_Node (sub_tree,left_biggest,m,(y,z),r))
          | Leaf -> raise Unreachable
        end
      | `LT -> let (sub_tree, shrunk) = remove_helper k1 l in
        if (not shrunk)
        then (Three_Node (sub_tree,(w,x), m, (y,z), r),false)
        else fix_tree sub_tree (Three_Node (sub_tree,(w,x), m, (y,z), r))
      | `GT ->
        begin
          match (C.compare k1 y) with
          | `EQ ->
            begin
              match m with
              | Two_Node (a,(sub1,val1),b) ->
                let left_biggest = find_biggest b in
                let (sub_tree, shrunk) = remove_helper (fst left_biggest) m in
                if (not shrunk)
                then (Three_Node (l,(w,x),sub_tree,left_biggest,r),false)
                else fix_tree sub_tree (Three_Node (l,(w,x),sub_tree,left_biggest,r))
              | Three_Node (a,(sub1,val1),b,(sub2,val2),c) ->
                let left_biggest = find_biggest c in
                let (sub_tree, shrunk) = remove_helper (fst left_biggest) m in
                if (not shrunk)
                then (Three_Node (l,(w,x),sub_tree,left_biggest,r),false)
                else fix_tree sub_tree (Three_Node (l,(w,x),sub_tree,left_biggest,r))
              | Leaf -> raise Unreachable
            end
          | `LT -> let (sub_tree, shrunk) = remove_helper k1 m in
            if (not shrunk)
            then (Three_Node (l,(w,x), sub_tree, (y,z), r),false) else
            fix_tree sub_tree (Three_Node (l,(w,x), sub_tree, (y,z), r))
          | `GT -> let (sub_tree, shrunk) = remove_helper k1 r in
            if (not shrunk)
            then (Three_Node (l,(w,x), m, (y,z), sub_tree),false) else
            fix_tree sub_tree (Three_Node (l,(w,x), m, (y,z), sub_tree))
        end
    end

  let remove k d =
    fst (remove_helper k d)


  let rec insert_helper k1 v1 d = match d with
  | Leaf -> (Two_Node (Leaf, (k1,v1), Leaf), true)
  | Two_Node (Leaf, (x,y), Leaf) ->
    begin
      match (C.compare k1 x) with
      | `EQ -> (Two_Node (Leaf, (k1,v1), Leaf), false)
      | `LT -> (Three_Node (Leaf, (k1,v1), Leaf, (x,y), Leaf), false)
      | `GT -> (Three_Node (Leaf, (x,y), Leaf, (k1,v1), Leaf), false)
    end
  | Three_Node (Leaf, (w,x), Leaf, (y,z), Leaf) ->
    begin
      match (C.compare k1 w) with
      | `EQ -> (Three_Node (Leaf, (k1,v1), Leaf, (y,z), Leaf), false)
      | `LT -> (Two_Node (Two_Node (Leaf, (k1,v1), Leaf), (w,x),
        Two_Node (Leaf, (y,z), Leaf)), true)
      | `GT ->
      begin
        match (C.compare k1 y) with
        | `EQ -> (Three_Node (Leaf, (k1,v1), Leaf, (y,z), Leaf), false)
        | `LT -> (Two_Node (Two_Node (Leaf, (w,x), Leaf), (k1,v1),
            Two_Node (Leaf, (y,z), Leaf)), true)
        | `GT -> (Two_Node (Two_Node (Leaf, (w,x), Leaf), (y,z),
            Two_Node (Leaf, (k1,v1), Leaf)), true)
      end
    end
  | Two_Node (l,(x,y),r) ->
    begin
      match (C.compare k1 x) with
      | `EQ -> (Two_Node (l,(k1,v1),r),false)
      | `LT -> let (sub_tree, kickback) = insert_helper k1 v1 l in
        if (not kickback) then (Two_Node (sub_tree,(x,y),r),false) else
        begin
          match sub_tree with
          | Two_Node (a, (sub1,val1), b) ->
              (Three_Node (a, (sub1,val1), b, (x,y), r), false)
          | Three_Node (a, (sub1,val1), b, (sub2,val2), c) ->
              (Two_Node (Two_Node (a, (sub1,val1),b), (sub2,val2),
              Two_Node (c, (x,y),r)),true)
          | Leaf -> raise Unreachable
        end
      | `GT -> let (sub_tree, kickback) = insert_helper k1 v1 r in
        if (not kickback) then (Two_Node (l,(x,y),sub_tree),false) else
        begin
          match sub_tree with
          | Two_Node (a, (sub1,val1), b) ->
              (Three_Node (l, (x,y), a, (sub1,val1), b), false)
          | Three_Node (a, (sub1,val1), b, (sub2,val2), c) ->
              (Two_Node (Two_Node (l, (x,y),a), (sub1,val1),
              Two_Node (b, (sub2,val2),c)),true)
          | Leaf -> raise Unreachable
        end
    end
  | Three_Node (l,(x1,y1),m,(x2,y2),r) ->
    begin
      match C.compare k1 x1 with
      | `EQ -> (Three_Node (l,(k1,v1),m,(x2,y2),r),false)
      | `LT -> let (sub_tree, kickback)= insert_helper k1 v1 l in
        if (not kickback)
        then (Three_Node (sub_tree,(x1,y1),m,(x2,y2),r),false)
        else
          begin
            match sub_tree with
            | Two_Node (a, (sub1,val1), b) ->
              (Two_Node (Two_Node (a,(sub1,val1),b),(x1,y1),
              Two_Node (m,(x2,y2),r)),true)
            | Three_Node (a, (sub1,val1), b, (sub2,val2), c) ->
              (Two_Node (Three_Node (a, (sub1,val1), b, (sub2,val2), c),(x1,y1),
              Two_Node (m,(x2,y2),r)),true)
            | Leaf -> raise Unreachable
          end
      | `GT ->
        begin
          match C.compare k1 x2 with
          | `EQ -> (Three_Node (l,(x1,y1),m,(k1,v1),r),false)
          | `LT -> let (sub_tree, kickback)= insert_helper k1 v1 m in
            if (not kickback)
            then (Three_Node (l,(x1,y1),sub_tree,(x2,y2),r),false)
            else
              begin
                match sub_tree with
                | Two_Node (a, (sub1,val1), b) ->
                  (Two_Node (Two_Node (l, (x1,y1), a), (sub1,val1),
                  Two_Node (b,(x2,y2),r)),true)
                | Three_Node (a, (sub1,val1), b, (sub2,val2), c) ->
                  (Two_Node (Three_Node (l, (x1,y1), a, (sub1,val1), b),
                  (sub2,val2),Two_Node (c,(x2,y2),r)),true)
                | Leaf -> raise Unreachable
              end
          | `GT -> let (sub_tree, kickback)= insert_helper k1 v1 r in
            if (not kickback)
            then (Three_Node (l,(x1,y1),m,(x2,y2),sub_tree),false)
            else
              begin
                match sub_tree with
              | Two_Node (a, (sub1,val1), b) ->
                (Two_Node (Two_Node (l, (x1,y1), m), (x2,y2),
                Two_Node (a,(sub1,val1),b)),true)
              | Three_Node (a, (sub1,val1), b, (sub2,val2), c) ->
                (Two_Node (Three_Node (l, (x1,y1), m, (x2,y2), a),(sub1,val1),
                Two_Node (b,(sub2,val2),c)),true)
              | Leaf -> raise Unreachable
              end
        end
    end

  let insert k v d =
    fst (insert_helper k v d)




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


