

module type Tree = sig
  type 'a t
  (*[insert] is the tree with 'a added in.
   requires: 'a t to be a valid tree *)
  val insert  : 'a t -> 'a -> 'a t

  (*[search] is the bool of if 'a is a member of 'a t
  requires: 'a t to be a valid tree*)
  val search  : 'a t -> 'a -> bool

  (*[peek] is the value of tree 'a t
  requires: 'a t to be a valid tree RNode*)
  val peek : 'a t -> 'a

  (*[is_empty] is the bool of if the tree is empty
  requires: 'a t to be a valid tree*)
  val is_empty: 'a t -> bool
end