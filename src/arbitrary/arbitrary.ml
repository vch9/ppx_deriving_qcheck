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
module Helpers = Common.Helpers
module Info = Helpers.Info
module Error = Common.Error
module T = Types_helper
module P = Common.Ast_helpers.Pattern
module PP = Common.Pp

let rec_flags = ref []

let extract_args ~loc params =
  let to_pat (ct, _) =
    match ct.ptyp_desc with
    | Ptyp_var s -> P.ppat_var ~loc @@ "arb_" ^ s
    | _ -> Error.case_unsupported ~loc ~case:"Ppx.Gen.gen.extract_args" ()
  in
  List.map to_pat params

let rec is_recursive ~loc ~ty = function
  | Ptype_variant cstrs ->
      List.exists (is_recursive_constructor_decl ~loc ~ty) cstrs
  | Ptype_record xs -> is_recursive_label_declarations ~loc ~ty xs
  | _ -> Error.case_unsupported ~case:"Ppx.Gen.is_recursive" ()

and is_recursive_constructor_decl ~loc ~ty cd =
  match cd.pcd_args with
  | Pcstr_tuple cts -> List.exists (is_recursive_core_type ~loc ~ty) cts
  | Pcstr_record xs -> is_recursive_label_declarations ~loc ~ty xs

and is_recursive_label_declarations ~loc ~ty xs =
  let labels =
    List.filter_map
      (fun x ->
        let loc = x.pld_type.ptyp_loc in
        match x.pld_type.ptyp_desc with
        | Ptyp_var s -> if s = ty then Some loc else None
        | Ptyp_constr (lg, _) ->
            let s = PP.longident_to_str lg.txt in
            if s = ty then Some loc else None
        | _ -> None)
      xs
  in
  match labels with
  | [] -> false
  | _ ->
      Error.location_error
        ~loc
        ~msg:"ppx_pbt does not supports recursive record"
        ()

and is_recursive_core_type ~loc ~ty ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = lg; _ }, cts) ->
      PP.longident_to_str lg = ty
      || List.exists (is_recursive_core_type ~loc ~ty) cts
  | Ptyp_variant (rws, _, _) -> is_recursive_row_fields ~loc ~ty rws
  | _ -> false

and is_recursive_row_field ~loc ~ty rw =
  match rw.prf_desc with
  | Rinherit _ ->
      Error.case_unsupported ~loc ~case:"Rinherit inside of row_field" ()
  | Rtag (_, _, cts) -> List.exists (is_recursive_core_type ~loc ~ty) cts

and is_recursive_row_fields ~loc ~ty rws =
  List.exists (is_recursive_row_field ~loc ~ty) rws

let rec from_core_type ~loc ?tree_types ?rec_types ~ty ct =
  match Attributes.arb ~loc ct with
  | Some x -> x
  | None -> (
      match ct.ptyp_desc with
      | Ptyp_constr ({ txt = ty; _ }, []) ->
          T.from_longident ~loc ?tree_types ?rec_types ty
      | Ptyp_constr ({ txt = x; _ }, args) ->
          let f = T.from_longident ~loc x in
          let args = List.map (from_core_type ~loc ?rec_types ~ty) args in
          T.constr_type ~loc ~f ~args ()
      | Ptyp_tuple xs -> from_tuple ~loc ~ty xs
      | Ptyp_var s -> T.Primitive.from_string ~loc s
      | Ptyp_variant (x, y, z) ->
          from_ptyp_variant ~loc ?tree_types ?rec_types ~ty (x, y, z)
      | _ -> Error.case_unsupported ~loc ~case:"Ppx.Gen.Types.from_core_type" ()
      )

(* [from_ptyp_variant] is not the same as [from_variant] *)
and from_ptyp_variant ~loc ?tree_types ?rec_types ~ty (rws, _, _) =
  (* Transforms a row_field to the pair (variant name, generators) *)
  let to_expr f rw =
    match rw.prf_desc with
    | Rtag ({ txt; _ }, _, cts) -> (txt, List.map f cts)
    | _ -> assert false
    (* If we get here, an exception should already have been raised *)
  in

  (* Standart transformation from core_type to generators *)
  let basic x = from_core_type ~loc ?tree_types ?rec_types ~ty x in

  if is_recursive_row_fields ~loc ~ty rws then (
    let is_leave x = not @@ is_recursive_row_field ~loc ~ty x in
    let leaves =
      List.filter is_leave rws
      |> List.map (to_expr basic)
      |> T.variants ~loc ~ty
    in
    let nodes =
      let tree_types' =
        Option.fold ~none:[ ty ] ~some:(fun x -> ty :: x) tree_types
        |> Option.some
      in
      let f = from_core_type ~loc ?tree_types:tree_types' ?rec_types ~ty in
      List.map (to_expr f) rws |> T.variants ~loc ~ty
    in
    rec_flags := ty :: !rec_flags ;
    T.tree' ~loc ~leaves ~nodes ())
  else List.map (to_expr basic) rws |> T.variants ~loc ~ty

and from_type_kind ~loc ?rec_types ~ty = function
  | Ptype_record xs -> from_record ~loc ?rec_types ~ty xs
  | Ptype_variant xs -> from_variant ~loc ?rec_types ~ty xs
  | _ -> failwith "TODO 1"

and from_record ~loc ?rec_types ~ty label_decls =
  let gens =
    List.map
      (fun x -> from_core_type ~loc ?rec_types ~ty x.pld_type)
      label_decls
  in
  T.record ~loc ~gens label_decls

and from_tuple ~loc ?rec_types ~ty cts =
  let gens = List.map (from_core_type ~loc ?rec_types ~ty) cts in
  T.tuple ~loc gens

and from_variant ~loc ?rec_types ~ty xs =
  if is_recursive ~loc ~ty @@ Ptype_variant xs then (
    let is_leave x = not @@ is_recursive_constructor_decl ~loc ~ty x in

    let leaves =
      List.filter is_leave xs
      |> List.map (fun x -> from_constructor_decl ~loc ?rec_types ~ty x)
    in
    let nodes =
      List.map
        (fun x ->
          from_constructor_decl ~loc ?rec_types ~tree_types:[ ty ] ~ty x)
        xs
    in

    rec_flags := ty :: !rec_flags ;

    T.tree ~loc ~leaves ~nodes ())
  else
    let gens = List.map (from_constructor_decl ~loc ?rec_types ~ty) xs in
    T.constructors ~loc gens

and from_constructor_decl ~loc ?tree_types ?rec_types ~ty x =
  let kname = x.pcd_name.txt in
  let f ~kargs = T.constructor ~loc ~kname ~kargs () in
  match x.pcd_args with
  | Pcstr_tuple [] | Pcstr_record [] -> T.constructor ~loc ~kname ()
  | Pcstr_tuple xs ->
      let gens = List.map (from_core_type ~loc ?tree_types ?rec_types ~ty) xs in
      let kargs = T.tuple' ~loc gens in
      f ~kargs
  | Pcstr_record xs ->
      let gens =
        List.map
          (fun x -> from_core_type ~loc ?tree_types ?rec_types ~ty x.pld_type)
          xs
      in
      let kargs = T.record' ~loc ~gens xs in
      f ~kargs

let from_type_declaration ~loc ?rec_types td =
  let ty = td.ptype_name.txt in

  let type_kind =
    Helpers.opt (fun () -> from_type_kind ~loc ?rec_types ~ty td.ptype_kind)
  in
  let body =
    match (td.ptype_manifest, type_kind) with
    (* We consider that type_kind contains the type information, and we take it
       over type_manifest *)
    | (_, Some x) -> x
    | (Some ct, None) -> from_core_type ~loc ?rec_types ~ty ct
    | (None, None) ->
        (* Getting here is because of 2 events:
           - type_kind raised an exception,
           - ptyp_manifest is absent *)
        assert false
  in

  let args = extract_args ~loc td.ptype_params in
  let rec_flags = !rec_flags in

  T.gen ~loc ~rec_flags ~args ~ty ~body ()

let from_type_declaration_rec ~loc xs =
  let tys = List.map (fun x -> x.ptype_name.txt) xs in
  let gens =
    List.map (fun x -> from_type_declaration ~loc ~rec_types:tys x) xs
  in
  T.gens ~loc ~tys ~gens ()
