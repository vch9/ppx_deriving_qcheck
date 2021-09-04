type tree = Leaf | Node of int * tree * tree [@@deriving qcheck]

let rec depth = function
  | Leaf -> 0
  | Node (_, left, right) -> max (depth left) (depth right)

(** [nodes tree] computes the number of node in [tree] *)
let rec nodes = function Leaf -> 0 | Node (_, x, y) -> 1 + nodes x + nodes y

(** [leaves tree] computes the number of leaf in [tree] *)
let rec leaves = function Leaf -> 1 | Node (_, x, y) -> leaves x + leaves y

(** [insert tree x] inserts [x] in [tree] *)
let insert tree x =
  let rec aux n = function
    | Leaf -> n
    | Node (elt, Leaf, right) -> Node (elt, n, right)
    | Node (elt, left, Leaf) -> Node (elt, left, n)
    | Node (elt, left, right) ->
        if depth left < depth right then Node (elt, aux n left, right)
        else Node (elt, left, aux n right)
  in
  aux (Node (x, Leaf, Leaf)) tree

(* [mem tree x] returns whether [x] is in [tree] or not *)
let rec mem tree x =
  match tree with
  | Leaf -> false
  | Node (y, left, right) -> x = y || mem left x || mem right x

let test_insert_nodes =
  QCheck.Test.make
    ~name:"(nodes x) + 1 = nodes (insert tree n)"
    QCheck.(pair (make gen_tree) small_int)
    (fun (tree, n) -> nodes tree + 1 = nodes (insert tree n))

let test_insert_mem =
  QCheck.Test.make
    ~name:"mem (insert tree n) n = true"
    QCheck.(pair (make gen_tree) small_int)
    (fun (tree, n) -> mem (insert tree n) n)

let _ =
  QCheck_base_runner.run_tests
    ~verbose:true
    [ test_insert_nodes; test_insert_mem ]
