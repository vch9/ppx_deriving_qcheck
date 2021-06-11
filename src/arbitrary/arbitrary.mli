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

(** Module expanding a type declaration with a QCheck.arbitrary *)

open Ppxlib

(** Transform a core_type into a QCheck.arbitrary

    When the tree_types contains types elements, that means they need to
    be an application of a generator

    A regular type would be translated to
    {[ fun s -> gen_s ]}

    Meanwhile a recursive type
    {[ fun s -> gen_s (n - 1) ]}
    Recursives types contains only one argument: the fuel. It is always
    called n

    When the rec_types contains types elements, that means they need to
    be wrapped with an additional argument ()

    The expression:
    {[
    let rec gen_expr = gen_expr' 5
    and gen_expr' = function
      | ..
      | ..
    and gen_value = gen_expr
    ]}

    is rejected because right-hand side of a 'let rec' definition doesn't
    accept that kind of expression. The hack is here is to change the signatures
    with an additional argument

    {[
    let rec gen_expr () = gen_expr' 5
    and gen_expr' = function
      | ..
      | ..
    and gen_value = gen_expr ()
    ]}
*)
val from_core_type :
  loc:location ->
  ?tree_types:string list ->
  ?rec_types:string list ->
  ty:string ->
  core_type ->
  expression

(** Transform a type kind into a QCheck.arbitrary
    
    - [X] type kind is a record, we use [from_record]
    - [X] type kind is a tuple, we use [from_record] *)
val from_type_kind :
  loc:location -> ?rec_types:string list -> ty:string -> type_kind -> expression

(** Transform a record into a record QCheck.arbitrary *)
val from_record :
  loc:location ->
  ?rec_types:string list ->
  ty:string ->
  label_declaration list ->
  expression

(** Transform a tuple into a tuple QCheck.arbitrary *)
val from_tuple :
  loc:location ->
  ?rec_types:string list ->
  ty:string ->
  core_type list ->
  expression

(** Transform a Ptype_variant into a 'a QCheck.arbitrary

    - [ ] the type is self recursive
      {[
      type tree = Leaf | Node of int * tree * tree
      ]}

      The distinction betweens recursive nodes and leaves must be considered
      in order to avoid a infinite loop on a recursive type

    - [X] the type is a list of constructor
      {[
      type color = Green | Blue | Red | Any of int
      ]}

      We just have to chose one of the constructors built using
      {!from_constructor_decl}. *)
val from_variant :
  loc:location ->
  ?rec_types:string list ->
  ty:string ->
  constructor_declaration list ->
  expression

(** Transform a constructor declaration into a 'a QCheck.arbitrary

    - [X] constructors without argument
      {[
      type t = A | B | C [@@gen]
      ]}

    - [X] constructors with argument

      - [X] argument as tuple
        {[
        type t = A of int | B of int * int
        ]}

      - [X] argument as record
        {[
        type t = A of { a : int }
        ]}

    ?rec_types allows optional additional information to {!from_core_type} *)
val from_constructor_decl :
  loc:location ->
  ?tree_types:string list ->
  ?rec_types:string list ->
  ty:string ->
  constructor_declaration ->
  expression

(** Transform a type declaration into a 'a QCheck.arbitrary *)
val from_type_declaration :
  loc:location -> ?rec_types:string list -> type_declaration -> structure_item

(** Transform recursive type declarations into multiple QCheck.arbitrary *)
val from_type_declaration_rec :
  loc:location -> type_declaration list -> structure_item
