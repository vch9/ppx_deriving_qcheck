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

let name s =
  let prefix = "gen" in
  match s with "t" -> prefix | s -> prefix ^ "_" ^ s

let pat ~loc s =
  let (module A) = Ast_builder.make loc in
  let s = name s in
  A.pvar s

let gen ~loc lg =
  let (module A) = Ast_builder.make loc in
  match lg with
  | Lident s -> name s |> A.evar
  | Ldot (lg, s) -> A.(pexp_construct (Located.mk @@ Ldot (lg, name s)) None)
  | Lapply (_, _) -> raise (Invalid_argument "gen received an Lapply")

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

  let to_pat ~loc t =
    let fresh_id =
      let id = ref 0 in
      fun () ->
        let x = !id in
        let () = id := x + 1 in
        Printf.sprintf "gen%d" x
    in
    let (module A) = Ast_builder.make loc in
    let rec aux = function
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
      | Pair (a, b) ->
          let a = aux a in
          let b = aux b in
          [%pat? ([%p a], [%p b])]
      | Elem _ -> A.pvar @@ fresh_id ()
    in
    aux t
end

let map ~loc pat expr gen = [%expr map (fun [%p pat] -> [%e expr]) [%e gen]]

let tuple ~loc ?(f = fun x -> x) tys =
  let tuple = Tuple.from_list tys in
  let gen = Tuple.to_gen ~loc tuple in
  let expr = Tuple.to_expr ~loc tuple |> f in
  let pat = Tuple.to_pat ~loc tuple in
  map ~loc pat expr gen

let rec gen_from_type ~loc typ =
  Option.value
    (Attributes.arb typ)
    ~default:
      (match typ with
      | [%type: unit] -> [%expr unit]
      | [%type: int] -> [%expr int]
      | [%type: string] | [%type: String.t] -> [%expr string]
      | [%type: char] -> [%expr char]
      | [%type: bool] -> [%expr bool]
      | [%type: float] -> [%expr float]
      | [%type: int32] | [%type: Int32.t] -> [%expr int32]
      | [%type: int64] | [%type: Int64.t] -> [%expr int64]
      | [%type: [%t? typ] option] -> [%expr option [%e gen_from_type ~loc typ]]
      | [%type: [%t? typ] list] -> [%expr list [%e gen_from_type ~loc typ]]
      | [%type: [%t? typ] array] -> [%expr array [%e gen_from_type ~loc typ]]
      | _ -> (
          match typ with
          | { ptyp_desc = Ptyp_tuple typs; _ } ->
              let tys = List.map (gen_from_type ~loc) typs in
              tuple ~loc tys
          | { ptyp_desc = Ptyp_constr ({ txt = ty; _ }, []); _ } -> gen ~loc ty
          | _ -> failwith "gen_from_type"))

and gen_from_variant ~loc xs =
  let (module A) = Ast_builder.make loc in
  let constr { pcd_name; pcd_args; pcd_attributes; _ } =
    let constr_decl =
      A.constructor_declaration ~name:pcd_name ~args:pcd_args ~res:None
    in
    let mk_constr expr = A.econstruct constr_decl (Some expr) in
    let weight = Attributes.weight pcd_attributes in
    let gen =
      match pcd_args with
      | Pcstr_tuple [] | Pcstr_record [] ->
          [%expr pure [%e A.econstruct constr_decl None]]
      | Pcstr_tuple xs ->
          let tys = List.map (gen_from_type ~loc) xs in
          tuple ~loc ~f:mk_constr tys
      | _ -> failwith "record"
    in
    A.pexp_tuple [ Option.value ~default:[%expr 1] weight; gen ]
  in

  let gens = List.map constr xs |> A.elist in

  [%expr frequency [%e gens]]

let gen ~loc td =
  let pat = pat ~loc td.ptype_name.txt in
  let gen =
    match td.ptype_kind with
    | Ptype_variant xs -> gen_from_variant ~loc xs
    | _ ->
        let typ = Option.get td.ptype_manifest in
        gen_from_type ~loc typ
  in
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
