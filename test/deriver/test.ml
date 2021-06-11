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

let test_option () =
  let expected =
    [
      [%stri
        let arb arb_a =
          QCheck.oneof
            [ QCheck.always None; QCheck.map (fun arb_0 -> Some arb_0) arb_a ]];
      [%stri let arb = arb_my_option QCheck.int];
      [%stri let arb = QCheck.option QCheck.int];
    ]
  in
  let actual =
    f'
    @@ extract'
         [
           [%stri type 'a t = None | Some of 'a];
           [%stri type t = int my_option];
           [%stri type t = int option];
         ]
  in
  check_eq ~expected ~actual "deriving option"

let test_list () =
  let expected =
    [
      [%stri let arb = QCheck.list QCheck.string];
      [%stri
        let arb =
          QCheck.oneof
            [
              QCheck.map (fun arb_0 -> A arb_0) (QCheck.list QCheck.string);
              QCheck.map (fun arb_0 -> B arb_0) (QCheck.list QCheck.int);
            ]];
    ]
  in

  let actual =
    f'
    @@ extract'
         [
           [%stri type t = string list];
           [%stri type t = A of string list | B of int list];
         ]
  in
  check_eq ~expected ~actual "deriving list"

let test_alpha () =
  let expected =
    [
      [%stri let arb arb_a = arb_a];
      [%stri let arb arb_a = QCheck.list arb_a];
      [%stri
        let arb arb_a = QCheck.oneof [ QCheck.map (fun arb_0 -> A arb_0) arb_a ]];
      [%stri
        let arb arb_a arb_b =
          QCheck.oneof
            [
              QCheck.map
                (fun (arb_0, arb_1) -> A (arb_0, arb_1))
                (QCheck.pair arb_a arb_b);
            ]];
      [%stri
        let arb arb_left arb_right =
          QCheck.map
            (fun (arb_0, arb_1) -> (arb_0, arb_1))
            (QCheck.pair arb_left arb_right)];
    ]
  in
  let actual =
    f'
    @@ extract'
         [
           [%stri type 'a t = 'a];
           [%stri type 'a t = 'a list];
           [%stri type 'a t = A of 'a];
           [%stri type ('a, 'b) t = A of 'a * 'b];
           [%stri type ('left, 'right) t = 'left * 'right];
         ]
  in
  check_eq ~expected ~actual "deriving alpha"

let test_equal () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.oneof [ QCheck.always A; QCheck.always B; QCheck.always C ]];
      [%stri
        let arb_t' =
          QCheck.oneof [ QCheck.always A; QCheck.always B; QCheck.always C ]];
    ]
  in
  let actual =
    f'
    @@ extract'
         [ [%stri type t = A | B | C]; [%stri type t' = t = A | B | C] ]
  in
  check_eq ~expected ~actual "deriving equal"

let test_dependencies () =
  let expected =
    [
      [%stri
        let arb =
          QCheck.oneof
            [
              QCheck.map (fun arb_0 -> Int arb_0) SomeModule.arb;
              QCheck.map
                (fun arb_0 -> Float arb_0)
                SomeModule.SomeOtherModule.arb;
            ]];
    ]
  in
  let actual =
    f
    @@ extract
         [%stri
           type t =
             | Int of SomeModule.t
             | Float of SomeModule.SomeOtherModule.t]
  in

  check_eq ~expected ~actual "deriving dependencies"

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
            test_case "deriving option" `Quick test_option;
            test_case "deriving list" `Quick test_list;
            test_case "deriving alpha" `Quick test_alpha;
            test_case "deriving equal" `Quick test_equal;
            test_case "deriving dependencies" `Quick test_dependencies;
          ] );
      ])
