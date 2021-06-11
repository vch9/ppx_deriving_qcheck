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

(** Module test for ppx_deriving_qcheck *)
open Ppxlib

(** Primitive types tests *)
let loc = Location.none

let f = Ppx_deriving_qcheck.derive_arbitrary ~loc

let f' xs = List.map f xs |> List.concat

let extract stri =
  match stri.pstr_desc with Pstr_type (x, y) -> (x, y) | _ -> assert false

let extract' xs = List.map extract xs

let check_eq ~expected ~actual name =
  let f = Ppxlib.Pprintast.string_of_structure in
  Alcotest.(check string) name (f expected) (f actual)

let test_int () =
  let expected = [ [%stri let arb = QCheck.int] ] in
  let actual = f @@ extract [%stri type t = int [@@deriving arb]] in

  check_eq ~expected ~actual "deriving int"

let test_float () =
  let expected = [ [%stri let arb = QCheck.float] ] in
  let actual = f @@ extract [%stri type t = float [@@deriving arb]] in

  check_eq ~expected ~actual "deriving float"

let test_char () =
  let expected = [ [%stri let arb = QCheck.char] ] in
  let actual = f @@ extract [%stri type t = char [@@deriving arb]] in

  check_eq ~expected ~actual "deriving char"

let test_string () =
  let expected = [ [%stri let arb = QCheck.string] ] in
  let actual = f @@ extract [%stri type t = string [@@deriving arb]] in

  check_eq ~expected ~actual "deriving string"

let test_unit () =
  let expected = [ [%stri let arb = QCheck.unit] ] in
  let actual = f @@ extract [%stri type t = unit [@@deriving arb]] in

  check_eq ~expected ~actual "deriving unit"

let test_bool () =
  let expected = [ [%stri let arb = QCheck.bool] ] in
  let actual = f @@ extract [%stri type t = bool [@@deriving arb]] in

  check_eq ~expected ~actual "deriving bool"

let test_int32 () =
  let expected = [ [%stri let arb = QCheck.int32] ] in
  let actual = f @@ extract [%stri type t = int32 [@@deriving arb]] in

  check_eq ~expected ~actual "deriving int32"

let test_int32' () =
  let expected = [ [%stri let arb = QCheck.int32] ] in
  let actual = f @@ extract [%stri type t = Int32.t [@@deriving arb]] in

  check_eq ~expected ~actual "deriving int32'"

let test_int64 () =
  let expected = [ [%stri let arb = QCheck.int64] ] in
  let actual = f @@ extract [%stri type t = int64 [@@deriving arb]] in

  check_eq ~expected ~actual "deriving int64"

let test_int64' () =
  let expected = [ [%stri let arb = QCheck.int64] ] in
  let actual = f @@ extract [%stri type t = Int64.t [@@deriving arb]] in

  check_eq ~expected ~actual "deriving int64'"

let test_bytes () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.map
            (fun n -> Bytes.create n)
            QCheck.(0 -- Sys.max_string_length)];
    ]
  in
  let actual = f @@ extract [%stri type t = Bytes.t [@@deriving arb]] in

  check_eq ~expected ~actual "deriving int64"

let test_tuple () =
  let actual =
    f'
    @@ extract'
         [
           [%stri type t = int * int];
           [%stri type t = int * int * int];
           [%stri type t = int * int * int * int];
           [%stri type t = int * int * int * int * int];
           [%stri type t = int * int * int * int * int * int];
         ]
  in
  let expected =
    [
      [%stri
        let arb =
          QCheck.map
            (fun (arb_0, arb_1) -> (arb_0, arb_1))
            (QCheck.pair QCheck.int QCheck.int)];
      [%stri
        let arb =
          QCheck.map
            (fun (arb_0, (arb_1, arb_2)) -> (arb_0, arb_1, arb_2))
            (QCheck.pair QCheck.int (QCheck.pair QCheck.int QCheck.int))];
      [%stri
        let arb =
          QCheck.map
            (fun ((arb_0, arb_1), (arb_2, arb_3)) ->
              (arb_0, arb_1, arb_2, arb_3))
            (QCheck.pair
               (QCheck.pair QCheck.int QCheck.int)
               (QCheck.pair QCheck.int QCheck.int))];
      [%stri
        let arb =
          QCheck.map
            (fun (arb_0, ((arb_1, arb_2), (arb_3, arb_4))) ->
              (arb_0, arb_1, arb_2, arb_3, arb_4))
            (QCheck.pair
               QCheck.int
               (QCheck.pair
                  (QCheck.pair QCheck.int QCheck.int)
                  (QCheck.pair QCheck.int QCheck.int)))];
      [%stri
        let arb =
          QCheck.map
            (fun ((arb_0, (arb_1, arb_2)), (arb_3, (arb_4, arb_5))) ->
              (arb_0, arb_1, arb_2, arb_3, arb_4, arb_5))
            (QCheck.pair
               (QCheck.pair QCheck.int (QCheck.pair QCheck.int QCheck.int))
               (QCheck.pair QCheck.int (QCheck.pair QCheck.int QCheck.int)))];
    ]
  in

  check_eq ~expected ~actual "deriving tuples"

let () =
  Alcotest.(
    run
      "ppx_deriving_qcheck tests"
      [
        ( "deriving arbitrary good",
          [
            test_case "deriving int" `Quick test_int;
            test_case "deriving float" `Quick test_float;
            test_case "deriving char" `Quick test_char;
            test_case "deriving string" `Quick test_string;
            test_case "deriving unit" `Quick test_unit;
            test_case "deriving bool" `Quick test_bool;
            test_case "deriving int32" `Quick test_int32;
            test_case "deriving int32'" `Quick test_int32';
            test_case "deriving int64" `Quick test_int64;
            test_case "deriving int64'" `Quick test_int64';
            test_case "deriving bytes" `Quick test_bytes;
            test_case "deriving tuple" `Quick test_tuple;
          ] );
      ])
