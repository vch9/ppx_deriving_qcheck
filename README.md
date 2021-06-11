# ppx_deriving_qcheck

## Arbitrary
Derive `QCheck.arbitrary` on a type declaration

```ocaml
type tree = Leaf of int | Node of tree * tree
[@@deriving arb]
```

### Overwrite arbitrary
If you wan't to specify your own `arbitrary` for any type you can
add an attribute to the type:

```ocaml
type t = (int : [@arb QCheck.(0 -- 10)])
[@@deriving arb]

(* produces *)

let arb : t QCheck.arbitrary = QCheck.(0 -- 10)
```

This attribute has 2 advantages:
* Use your own arbitrary for a specific type
* Type is not available for a module
  ```ocaml
  type t = Time.t [@@deriving arb]
                  ^^^^^^^^^^^^^^^^
  Error: Unbound value Time.arb
  
  (* Possible fix *)
  let arb_time = Obj.magic
  
  type t = (Time.t [@arb arb_time])
  [@@deriving arb]
  ```

## How to use
Install ppx_qcheck (dev version)
```
$ opam pin install ppx_qcheck git+https//github.com/vch9/ppx_qcheck.git#dev
```

Add to your OCaml libraries with dune
```ocaml
...
(libraries ppx_deriving_qcheck)
(preprocess (pps ppx_deriving_qcheck)))
...
```
