# ppx_deriving_qcheck

## Generate 'a arbitrary

## How to use
Install ppx_qcheck (dev version)
```
$ opam pin install ppx_qcheck git+https//github.com/vch9/ppx_qcheck.git#dev
```

Add to your OCaml libraries with dune
```ocaml
(library
 (name foo)
 (libraries ppx_deriving_qcheck)
 (preprocess (pps ppx_deriving_qcheck)))
```
