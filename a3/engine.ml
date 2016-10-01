exception Unimplemented

module type Engine = sig
  type idx
  val index_of_dir : string -> idx
  val to_list : idx -> (string * string list) list
  val or_not  : idx -> string list -> string list -> string list
  val and_not : idx -> string list -> string list -> string list
  val format : Format.formatter -> idx -> unit
end

module MakeEngine
  (S:Data.Set with type Elt.t = string)
  (D:Data.Dictionary with type Key.t = string)
  : Engine
=
struct
  type idx = unit

  let index_of_dir d =
    raise Unimplemented

  let to_list idx =
    raise Unimplemented

  let or_not idx ors nots =
    raise Unimplemented

  let and_not idx ands nots =
    raise Unimplemented

  let format fmt idx =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end

module ListEngine =
(* TODO: delete the entire struct below and replace it with
   a call to [MakeEngine] on some appropriate parameters. *)
struct
  type idx = unit
  let index_of_dir d = ()
  let to_list idx = []
  let or_not idx ors nots = []
  let and_not idx ands nots = []
  let format fmt idx = ()
end

(* TODO (later): after you've implemented 2-3 trees,
   replace this definition of [TreeEngine] with one
   that calls [MakeEngine] on some appropriate parameters.
   For now, this code punts by equating the two modules. *)
module TreeEngine = ListEngine


