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

open Ppxlib

(* TODO:

   - For now my function returned a single structure_item (that is actually a
   include struct with a structure inside. But we can now return a structure now
*)

let pat ~loc s =
  let prefix = "gen" in
  let s = match s with "t" -> prefix | s -> prefix ^ "_" ^ s in
  let (module A) = Ast_builder.make loc in
  A.pvar s

let built_in_opt ~loc = function
  | [%type: unit] -> Some [%expr unit]
  | [%type: int] -> Some [%expr int]
  | [%type: string] | [%type: String.t] -> Some [%expr string]
  | [%type: char] -> Some [%expr char]
  | [%type: bool] -> Some [%expr bool]
  | [%type: float] -> Some [%expr float]
  | [%type: int32] | [%type: Int32.t] -> Some [%expr int32]
  | [%type: int64] | [%type: Int64.t] -> Some [%expr int64]
  (* | [%type: option] -> Some [%expr option]
   * | [%type: list] -> Some [%expr list]
   * | [%type: array] -> Some [%expr array] *)
  | _ -> None

module Tuple = struct
  type 'a t =
    | Pair of 'a t * 'a t
    | Triple of 'a * 'a * 'a
    | Quad of 'a * 'a * 'a * 'a
    | Elem of 'a

  let rec from_list = function
    | [ a; b; c; d ] -> Quad (a, b, c, d)
    | [ a; b; c ] -> Triple (a, b, c)
    | [ a; b ] -> Pair (Elem a, Elem b)
    | [ a ] -> Elem a
    | l ->
        let n = List.length l / 2 in
        let l1 = List.filteri (fun i _ -> i < n) l in
        let l2 = List.filteri (fun i _ -> i >= n) l in
        Pair (from_list l1, from_list l2)

  let rec to_list = function
    | Quad (a, b, c, d) -> [ a; b; c; d ]
    | Triple (a, b, c) -> [ a; b; c ]
    | Pair (a, b) -> to_list a @ to_list b
    | Elem a -> [ a ]

  let to_expr ~loc t =
    let l = to_list t in
    let (module A) = Ast_builder.make loc in
    List.mapi
      (fun i _ ->
        let s = Printf.sprintf "gen%d" i in
        A.evar s)
      l
    |> A.pexp_tuple

  let rec to_gen ~loc = function
    | Quad (a, b, c, d) -> [%expr quad [%e a] [%e b] [%e c] [%e d]]
    | Triple (a, b, c) -> [%expr triple [%e a] [%e b] [%e c]]
    | Pair (a, b) -> [%expr pair [%e to_gen ~loc a] [%e to_gen ~loc b]]
    | Elem a -> a

  let rec to_pat ~loc t =
    let fresh_id =
      let id = ref 0 in
      fun () ->
        let x = !id in
        let () = id := x + 1 in
        Printf.sprintf "gen%d" x
    in
    let (module A) = Ast_builder.make loc in
    match t with
    | Quad (_, _, _, _) ->
        let a = A.pvar @@ fresh_id () in
        let b = A.pvar @@ fresh_id () in
        let c = A.pvar @@ fresh_id () in
        let d = A.pvar @@ fresh_id () in
        [%pat? ([%p a], [%p b], [%p c], [%p d])]
    | Triple (_, _, _) ->
        let a = A.pvar @@ fresh_id () in
        let b = A.pvar @@ fresh_id () in
        let c = A.pvar @@ fresh_id () in
        [%pat? ([%p a], [%p b], [%p c])]
    | Pair (a, b) -> [%pat? ([%p to_pat ~loc a], [%p to_pat ~loc b])]
    | Elem _ -> A.pvar @@ fresh_id ()
end

let map ~loc pat expr gen = [%expr map (fun [%p pat] -> [%e expr]) [%e gen]]

let tuple ~loc tys =
  let tuple = Tuple.from_list tys in
  let gen = Tuple.to_gen ~loc tuple in
  let expr = Tuple.to_expr ~loc tuple in
  let pat = Tuple.to_pat ~loc tuple in
  map ~loc pat expr gen

let rec gen_from_type ~loc typ =
  match (Attributes.arb typ, built_in_opt ~loc typ) with
  | (Some x, _) | (None, Some x) -> x
  | (None, None) -> (
      match typ with
      | { ptyp_desc = Ptyp_tuple typs; _ } ->
          let tys = List.map (gen_from_type ~loc) typs in
          tuple ~loc tys
      | _ -> failwith "todo")

let gen ~loc td =
  let pat = pat ~loc td.ptype_name.txt in
  let gen = gen_from_type ~loc (Option.get td.ptype_manifest) in

  [%stri
    let [%p pat] =
      let open QCheck in
      let open Gen in
      [%e gen]]

let derive_arbitrary ~loc xs : structure =
  match xs with
  | (_, [ x ]) -> [gen ~loc x]
  | (_, _xs) -> assert false
  [@@ocamlformat "disable"]
(* [ Arbitrary.from_type_declarations ~loc xs ] *)

let create_arbitrary ~ctxt (decls : rec_flag * type_declaration list) :
    structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  derive_arbitrary ~loc decls

let arb_generator = Deriving.Generator.V2.make_noarg create_arbitrary

let _ = Deriving.add "arb" ~str_type_decl:arb_generator
