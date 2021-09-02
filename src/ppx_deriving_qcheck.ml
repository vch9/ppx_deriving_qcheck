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

(** TypeGen can serve as a derivation environment. The map can be used
    to remember how a type should be translated.

    For instance, a recursive type must be derivated to a self recursive
    call reducing the size.

    {[
    type tree = Leaf of int | Node of tree * tree
    ]}

    becomes

    {[
    let gen_tree =
      let open QCheck in
      let open Gen in
      sized
        @@ fix (fun self -> function
             | 0 -> frequency [ (1, pure Leaf) ]
             | n ->
                 frequency
                   [
                     (1, pure Leaf);
                     ( 1,
                       map
                         (fun (gen0, gen1, gen2) -> Node (gen0, gen1, gen2))
                         (triple int (self (n / 1)) (self (n / 1))) );])
    ]}

    The type [tree] is stored in a TypeGen.t with tree <- [%expr self (n/2)]. This
    avoids the case where [tree] is derivated to [gen_tree]
*)
module TypeGen = Map.Make (struct
  type t = string

  let compare = compare
end)

let rec longident_to_str = function
  | Lident s -> s
  | Ldot (lg, s) -> Printf.sprintf "%s.%s" (longident_to_str lg) s
  | Lapply (lg1, lg2) ->
      Printf.sprintf "%s %s" (longident_to_str lg1) (longident_to_str lg2)

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

let record ~loc ~gens ?(f = fun x -> x) xs =
  let (module A) = Ast_builder.make loc in
  let tuple = Tuple.from_list gens in
  let gen = Tuple.to_gen ~loc tuple in
  let pat = Tuple.to_pat ~loc tuple in
  let gens =
    List.mapi
      (fun i _ ->
        let s = Printf.sprintf "gen%d" i in
        A.evar s)
      gens
  in
  let fields =
    List.map2
      (fun { pld_name; _ } value ->
        (A.Located.mk @@ Lident pld_name.txt, value))
      xs
      gens
  in
  let expr = A.pexp_record fields None |> f in

  map ~loc pat expr gen

let rec gen_from_type ~loc ?(env = TypeGen.empty) typ =
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
          | { ptyp_desc = Ptyp_constr ({ txt = ty; _ }, _); _ } ->
              let x = TypeGen.find_opt (longident_to_str ty) env in
              Option.value ~default:(gen ~loc ty) x
          | { ptyp_desc = Ptyp_var s; _ } -> gen ~loc (Lident s)
          | _ -> failwith "gen_from_type"))

and gen_from_constr ~loc ?(env = TypeGen.empty)
    { pcd_name; pcd_args; pcd_attributes; _ } =
  let (module A) = Ast_builder.make loc in
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
        let tys = List.map (gen_from_type ~loc ~env) xs in
        tuple ~loc ~f:mk_constr tys
    | Pcstr_record xs ->
        let tys = List.map (fun x -> gen_from_type ~loc x.pld_type) xs in
        record ~loc ~f:mk_constr ~gens:tys xs
  in

  A.pexp_tuple [ Option.value ~default:[%expr 1] weight; gen ]

and gen_from_variant ~loc typ_name xs =
  let (module A) = Ast_builder.make loc in
  let is_rec (constr : constructor_declaration) : bool =
    match constr.pcd_args with
    | Pcstr_tuple xs ->
        List.exists
          (function
            | { ptyp_desc = Ptyp_constr ({ txt = x; _ }, _); _ } ->
                longident_to_str x = typ_name
            | _ -> false)
          xs
    | _ -> false
  in

  let leaves =
    List.filter (fun x -> not (is_rec x)) xs |> List.map (gen_from_constr ~loc)
  in
  let nodes = List.filter is_rec xs in

  if List.length nodes > 0 then
    let env = TypeGen.singleton typ_name [%expr self (n / 2)] in
    let nodes = nodes |> List.map (gen_from_constr ~loc ~env) in
    let leaves = A.elist leaves and nodes = A.elist (leaves @ nodes) in
    [%expr
      sized
      @@ fix (fun self -> function
           | 0 -> frequency [%e leaves] | n -> frequency [%e nodes])]
  else
    let gens = A.elist leaves in
    [%expr frequency [%e gens]]

let rec curry_args ~loc args body =
  match args with
  | [] -> body
  | x :: xs -> [%expr fun [%p x] -> [%e curry_args ~loc xs body]]

let gen ~loc td =
  let name = td.ptype_name.txt in
  let pat_gen = pat ~loc name in

  let args =
    List.map
      (fun (typ, _) ->
        match typ.ptyp_desc with Ptyp_var s -> pat ~loc s | _ -> assert false)
      td.ptype_params
  in

  let gen =
    match td.ptype_kind with
    | Ptype_variant xs -> gen_from_variant ~loc name xs
    | Ptype_record xs ->
        let gens = List.map (fun x -> gen_from_type ~loc x.pld_type) xs in
        record ~loc ~gens xs
    | _ ->
        let typ = Option.get td.ptype_manifest in
        gen_from_type ~loc typ
  in
  let gen = curry_args ~loc args gen in

  [%stri
    let [%p pat_gen] =
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
