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
module Error = Common.Error
module E = Common.Ast_helpers.Expression
module P = Common.Ast_helpers.Pattern
module S = Common.Ast_helpers.Structure
module T = Common.Ast_helpers.Type
module H = Common.Helpers
module Pairs = Common.Helpers.Pairs
module PP = Common.Pp

let name s = match s with "t" -> "arb" | s -> Printf.sprintf "arb_%s" s

module Primitive = struct
  let from_string ~loc ?tree_types ?rec_types = function
    | "int" -> [%expr QCheck.int]
    | "string" -> [%expr QCheck.string]
    | "char" -> [%expr QCheck.char]
    | "bool" -> [%expr QCheck.bool]
    | "float" -> [%expr QCheck.float]
    | "unit" -> [%expr QCheck.unit]
    | "option" -> [%expr QCheck.option]
    | "list" -> [%expr QCheck.list]
    | "int64" -> [%expr QCheck.int64]
    | "int32" -> [%expr QCheck.int32]
    | s ->
        let arb = E.pexp_lident ~loc @@ name s in

        let arb_opt =
          match (tree_types, rec_types) with
          | (Some xs, _) when List.mem s xs ->
              Some
                (E.pexp_apply
                   ~loc
                   ~f:(E.pexp_lident ~loc @@ name (s ^ "'"))
                   ~args:[ (Nolabel, [%expr n - 1]) ]
                   ())
          | (_, Some xs) when List.mem s xs ->
              Some (E.pexp_apply ~loc ~f:arb ~args:[ (Nolabel, [%expr ()]) ] ())
          | _ -> None
        in
        Option.fold ~none:arb ~some:(fun x -> x) arb_opt
end

let constr_type ~loc ~f ~args () =
  let args = List.map (fun x -> (Nolabel, x)) args in
  E.pexp_apply ~loc ~f ~args ()

let from_longident ~loc ?tree_types ?rec_types = function
  | Lident s -> Primitive.from_string ~loc ?tree_types ?rec_types s
  | Ldot (lg, s) as x -> (
      match PP.longident_to_str x with
      | "Int64.t" -> [%expr QCheck.int64]
      | "Int32.t" -> [%expr QCheck.int32]
      | "Bytes.t" ->
          [%expr
            QCheck.map
              (fun n -> Bytes.create n)
              QCheck.(0 -- Sys.max_string_length)]
      | _ -> E.pexp_ident ~loc @@ H.mk_loc ~loc @@ Ldot (lg, name s))
  | _ ->
      Error.case_unsupported
        ~loc
        ~case:"Ppx.Gen.Types.create_gen_from_longident"
        ()

let nest_gens ~loc gens =
  let open Pairs in
  let gens = nest_generators gens in
  let (pat, gens_name) = pattern_from_gens loc (fun x -> "arb_" ^ x) gens in
  let gens = nested_pairs_to_expr loc gens in
  let gens_name = nested_pairs_to_list gens_name in

  (gens, gens_name, pat)

let record' ~loc ~gens xs =
  let (gens_expr, gens_name, gens_pat) = nest_gens ~loc gens in
  let fields =
    List.map2
      (fun x gen ->
        let name = x.pld_name.txt in
        (H.mk_loc ~loc (Lident name), E.pexp_lident ~loc gen))
      xs
      gens_name
  in
  let body = E.pexp_record ~loc ~fields None in
  (gens_pat, gens_expr, body)

let record ~loc ~gens xs =
  let (gens_pat, gens_expr, body) = record' ~loc ~gens xs in
  [%expr QCheck.map (fun [%p gens_pat] -> [%e body]) [%e gens_expr]]

let tuple' ~loc gens =
  let (gens_expr, gens_name, gens_pat) = nest_gens ~loc gens in
  let body = List.map E.(pexp_lident ~loc) gens_name |> E.pexp_tuple ~loc in
  (gens_pat, gens_expr, body)

let tuple ~loc gens =
  let (gens_pat, gens_expr, body) = tuple' ~loc gens in
  [%expr QCheck.map (fun [%p gens_pat] -> [%e body]) [%e gens_expr]]

let constructors ~loc xs =
  let xs = E.pexp_list ~loc xs in
  [%expr QCheck.oneof [%e xs]]

let constructor ~loc ~kname ?kargs () =
  let kname = H.mk_loc ~loc @@ Lident kname in

  match kargs with
  | None ->
      let kname = E.pexp_construct ~loc ~kname ~kargs:None () in
      [%expr QCheck.always [%e kname]]
  | Some (pat, gens, expr) ->
      let expr = E.pexp_construct ~loc ~kname ~kargs:(Some expr) () in
      [%expr QCheck.map (fun [%p pat] -> [%e expr]) [%e gens]]

let tree' ~loc ~leaves ~nodes () =
  let rec_gen = [%expr function 0 -> [%e leaves] | n -> [%e nodes]] in
  rec_gen

let tree ~loc ~leaves ~nodes () =
  let leaves = E.pexp_list ~loc leaves and nodes = E.pexp_list ~loc nodes in

  let rec_gen =
    [%expr
      function 0 -> QCheck.oneof [%e leaves] | n -> QCheck.oneof [%e nodes]]
  in
  rec_gen

let variants' ~loc xs =
  List.map
    (fun (label, gens) ->
      match gens with
      | [] -> [%expr QCheck.always [%e E.pexp_variant ~loc ~label None]]
      | gens ->
          let (pat, gens, tuple) = tuple' ~loc gens in
          let expr = E.pexp_variant ~loc ~label @@ Some tuple in
          [%expr QCheck.map (fun [%p pat] -> [%e expr]) [%e gens]])
    xs

let variants ~loc ~ty xs =
  let xs = variants' ~loc xs in
  (* OCaml can not easily generalize types of ptyp_variants, we help the typer
     with an annotation *)
  let exp = constructors ~loc xs in
  let annotation =
    T.constr_one ~loc (Ldot (Lident "QCheck", "arbitrary")) (Lident ty)
  in

  E.pexp_constraint ~loc exp annotation

let rec curry_args ~loc args body =
  match args with
  | [] -> body
  | x :: xs -> [%expr fun [%p x] -> [%e curry_args ~loc xs body]]

let gen ~loc ~rec_flags ~args ~ty ~body () =
  let body = curry_args ~loc args body in
  let pat_name = P.ppat_var ~loc (name ty) in
  let ty_in_recs = List.mem ty rec_flags in

  if not ty_in_recs then [%stri let [%p pat_name] = [%e body]]
  else
    let name' = name ty ^ "'" in
    let pat_name' = P.ppat_var ~loc name' in
    let f = E.pexp_lident ~loc name' in
    let args = [ (Nolabel, [%expr 5]) ] in
    let unit = [ (Nolabel, E.unit ~loc ()) ] in
    let pat_expr = E.pexp_apply ~loc ~f ~args () in

    [%stri
      include struct
        let rec [%p pat_name] = fun () -> [%e pat_expr]

        and [%p pat_name'] = [%e body]

        let [%p pat_name] =
          [%e E.pexp_apply ~loc ~f:(E.pexp_lident ~loc (name ty)) ~args:unit ()]
      end]

(** We wan't to recognize that kind of expression:

    {[
    include struct
      let rec gen_something () = gen_something' 5
      and gen_something' = ...

      let gen_something = gen_something ()
    end
    ]}

    But following pattern does not catch the expression for a unknown reason:
    {[
    [%stri
      include struct
        [%stri let rec [%p? _] = [%e? _]
               and     [%p? _] = [%e? _]
               
               let [%? _] = [%e? _]]
      end
    ]}

    The solution is to make the pattern by hand going throught the structure *)
let stri_self_rec stri =
  match stri.pstr_desc with
  | Pstr_include { pincl_mod = { pmod_desc = Pmod_structure xs; _ }; _ } -> (
      let n = List.length xs in
      if n <> 2 then None
      else
        let x = List.hd xs in
        let y = List.nth xs 1 in

        match (x, y) with
        | ( [%stri
              let rec [%p? p] = [%e? e]

              and [%p? p'] = [%e? e']],
            [%stri let [%p? _] = [%e? _]] ) ->
            Some [ (p, e); (p', e') ]
        | _ -> None)
  | _ -> None

(** When a structure item is inside a mutual recursion definition,
    by convention we change the signature from
    {[ let f = .. ]}
    to
    {[ let f () = .. ]}

    This function changes the signature when [f] does not contains
    the suffix "'". *)
let change_sig ~loc (pat, expr) =
  let name = P.extract_pat_name_exn ~loc pat in
  let n = String.length name - 1 in
  let c = String.get name n in

  if c = '\'' then (pat, expr)
  else
    match expr with
    (* The expression already contains the fun () -> _ *)
    | [%expr fun () -> [%e? _]] ->
        (pat, expr) (* Otherwise we add the fun () -> _ *)
    | _ -> (pat, [%expr fun () -> [%e expr]])

let gens ~loc ~tys ~gens () =
  let real_gens =
    List.map
      (fun x ->
        let x = name x in
        let pat_name = P.ppat_var ~loc x in
        let f = E.pexp_lident ~loc x in
        let unit = (Nolabel, E.unit ~loc ()) in

        [%stri let [%p pat_name] = [%e E.pexp_apply ~loc ~f ~args:[ unit ] ()]])
      tys
  in

  let vbs =
    List.map
      (function
        | [%stri let [%p? pat] = [%e? body]] -> [ (pat, body) ]
        | stri -> Option.fold ~none:[] ~some:(fun x -> x) @@ stri_self_rec stri)
      gens
    |> List.flatten
    |> List.map (change_sig ~loc)
    |> List.map (fun (pat, e) -> S.value_binding ~loc ~pat e)
  in
  let aux_gens = S.structure_item ~loc @@ Pstr_value (Recursive, vbs) in

  S.str_include ~loc @@ aux_gens :: real_gens
