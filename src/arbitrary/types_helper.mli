(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Module helping with OCaml types as QCheck.arbitrary *)

open Ppxlib

(** Convention generator name for any type name *)
val name : string -> string

(** Module representing OCaml primitives types supported *)
module Primitive : sig
  (** Convert string name of type into QCheck arbitrary

      rec_types cover the special case where the generator is a recursive
      function

      tree_types cover the special case where the generator is a self recursive
      function*)
  val from_string :
    loc:location ->
    ?tree_types:string list ->
    ?rec_types:string list ->
    string ->
    expression
end

(** Convert an applicated parametrizable type into a QCheck arbitrary 

    In this example, the type is an parametrizable list applicated to string:
    {[
    type t = string list [@@gen]

    let gen_t = QCheck.list QCheck.string
    ]}
*)
val constr_type :
  loc:location -> f:expression -> args:expression list -> unit -> expression

(** Transform a longident into a QCheck.arbitrary

    Multiples cases:
    - [X] the type is a identifier, either from a Primitive type or local scope
    - [X] the type comes from an outside module, we require a generator inside that
    outside module
    - [ ] the type is an application, can that type happens ?

    rec_types cover the special case where the generator is a recursive
    function

    tree_types cover the special case where the generator is a self recursive
    function *)
val from_longident :
  loc:location ->
  ?tree_types:string list ->
  ?rec_types:string list ->
  longident ->
  expression

(** Transform list of generators into a triple:
    
    - expressions nested with QCheck.pair expression
    - generators names used in the expression
    - pattern according to the nested expression *)
val nest_gens :
  loc:location -> expression list -> expression * string list * pattern

(** Record auxiliar function, it extracts the pattern for a constructor application,
    the list of generators needed and the record expression
    
    type t = {left : int ; right = string }
    
    record' [int; string] [ left -> int ; right -> string ] =>
      - pattern : (x, y)
      - generators : pair QCheck.int QCheck.string
      - expression : { left = x ; right = string }

    This function has to be extracted from [record] because when trying to build
    a constructor using record, this expression needs the direct
    declaration of the record.

    Example
    {[
    type t = A of { something : int }
    
    let gen_t =
      QCheck.map (fun x -> A { something = x }) QCheck.int
    ]}

    In order to build such a QCheck.arbitrary we need the pattern, generators
    and record *)
val record' :
  loc:location ->
  gens:expression list ->
  label_declaration list ->
  pattern * expression * expression

(** Convert generators and labels declarations in a application of a record type

    Example:
    {[
    type t = { left : int ; right : string } [@@gen]

    let gen_t =
      QCheck.map (fun (x,y) -> { left = x ; right = y }) (QCheck.pair QCheck.int QCheck.string)
    ]}
 *)
val record :
  loc:location -> gens:expression list -> label_declaration list -> expression

(** Tuple auxiliar function, it extracts the pattern for a constructor application,
    the list of generators needed and the record expression
    
    type t = int * string
    
    record' [int; string] =>
      - pattern : (x, y)
      - generators : pair QCheck.int QCheck.string
      - expression : (int * string)

    This function has to be extracted from [tuple] because when trying to build
    a constructor using tuple, this expression needs the direct
    declaration of the record.
    
    Example
    {[
    type t = A of int * string
    
    let gen_t =
      QCheck.map (fun (x,y) -> A (int, string) ) QCheck.(pair int string)
    ]}

    In order to build such a QCheck.arbitrary we need the pattern, generators
    and record *)
val tuple' :
  loc:location -> expression list -> pattern * expression * expression

(** Convert generators in a application of a tuple
    
    Example:
    {[
    type t = int * int * int
    
    let gen_t =
      QCheck.map (fun (x,y,z) -> (x,y,z)) (QCheck.triple int int int)
    ]}
*)
val tuple : loc:location -> expression list -> expression

(** [constructors loc xs] convert a list of (weight option * constructor) into a single
    expression choosing the constructor using it's weight (1 if it's not provided).

    Example:
    {[
    type t =
    | A [@weight 5]
    | B [@weight 6]
    | C
    [@@gen]

    let gen_t =
      let open QCheck in
      frequency [ (5, always A) ;
                  (6, always B) ;
                  (1, always C) ]
    ]}
*)
val constructors :
  loc:location -> (expression option * expression) list -> expression

(** Convert a constructor name into an expression constructor QCheck.arbitrary

    Example:
    {[
    type t =
    | A

    (* A => QCheck.make @@ QCheck.Gen.return A *)
    ]}

    An additional case is supported when constructor requires arguments
    
    Example:
    {[
    type t =
    | A of int * string
    | B of { left : int ; right : string }

    (* A => QCheck.map (fun (x,y) -> A (x,y)) QCheck.(pair int string)
       B => QCheck.map (fun (x,y) -> B { left = x ; right ; y }) QCheck.(pair int string) *)
    ]}
*)
val constructor :
  loc:location ->
  kname:string ->
  ?kargs:pattern * expression * expression ->
  unit ->
  expression

(** [tree' loc leaves nodes ()] is almost the same function as {!tree'}
    the only difference is that QCheck.frequency is already applied to leaves and
    nodes *)
val tree' :
  loc:location -> leaves:expression -> nodes:expression -> unit -> expression

(** Convert a tree type like into a recursive generator expression

    The recursive generator uses a fuel, we could imagine that in future work
    the fuel would be provided by the user.

    Example:
    {[
    type t = Tree | Node of int * leaf * leaf
    [@@gen]

    let rec gen_tree fuel =
      let open QCheck in
      match fuel with
      | 0 -> frequency [(1, always Leaf)]
      | n ->
        frequency
          [
            (1, always Leaf) ;
            (1, map
              (fun (gen_0, (gen_1, gen_2)) -> Node (gen_0, gen_1, gen_2))
              (pair int
                 (pair (gen_tree (n - 1)) (gen_tree (n - 1)))))
          ]

    let gen_tree = gen_tree 5
    ]}
 *)
val tree :
  loc:location ->
  leaves:(expression option * expression) list ->
  nodes:(expression option * expression) list ->
  unit ->
  expression

(** [variants loc ty xs] create a QCheck.arbitrary using [xs] to produce
    Ptyp_variant _ list. We also require [ty] to constraint the expression
    to a {[ t QCheck.arbitrary ]} where {[ t ]} is the current type we
    are deriving.

    `RTag represents the direct declaration of a variant {[type t = [`A]]}
    `RInh represents an inheritage of another variant {[type t' = [`B | t]]}

    Each variant can have a specific weight, see {!constructors}. *)
val variants :
  loc:location ->
  ty:string ->
  [< `RTag of string * expression option * expression list
  | `RInh of expression option * expression ]
  list ->
  expression

(** Create a QCheck.arbitrary using args name and body

    let name args = body

    if there is a rec flag for the type, we add the recursive flag:
    let rec name args = body *)
val gen :
  loc:location ->
  rec_flags:string list ->
  args:pattern list ->
  ty:string ->
  body:expression ->
  unit ->
  structure_item

val gens :
  loc:location ->
  tys:string list ->
  gens:structure_item list ->
  unit ->
  structure_item

(** [observable loc x] create an QCheck.Observable.t if [x] is an observable *)
val observable : loc:location -> core_type -> expression

(** [fun_nary loc obs x] create an ('a Tuple.t -> 'b) fun_ arbitrary *)
val fun_nary : loc:location -> expression list -> expression -> expression
