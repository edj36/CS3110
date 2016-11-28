
module RadixTree: Tree = struct

  type 'a t =
  |RLeaf
  |RNode of 'a * 'a t * 'a t

  let empty = RLeaf

  (*[is_empty] is the bool of if the tree is empty*)
  let is_empty t  = failwith "Unimplemented"

  (*[peek] is the value of tree 'a t
  requires: 'a t to be a valid tree RNode*)
  let peek t = failwith "Unimplemented"


  (*[insert] is the tree with 'a added in*)
  let insert t s = failwith "Unimplemented"


  (*[search] is the bool of if 'a is a member of 'a t*)
  let search t s = failwith "Unimplemented"


end
