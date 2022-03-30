(lang dune 2.8)
(name arg-complete)
(generate_opam_files true)
(source (github sim642/ocaml-arg-complete))
(documentation https://sim642.github.io/ocaml-arg-complete)
(authors "Simmo Saan <simmo.saan@gmail.com>")
(maintainers "Simmo Saan <simmo.saan@gmail.com>")
(license MIT)

(package
 (name arg-complete)
 (synopsis "Bash completion support for Stdlib.Arg")
 (depends
  (cppo (>= 1.1.0))
  (ounit2 :with-test)))
