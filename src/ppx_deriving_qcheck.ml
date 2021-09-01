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

let gen_from_type ~loc typ =
  match (Attributes.arb typ, built_in_opt ~loc typ) with
  | (Some x, _) | (None, Some x) -> x
  | (None, None) -> failwith "todo"

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
