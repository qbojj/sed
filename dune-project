(lang dune 3.16)

(name sed)

(generate_opam_files true)

(source
 (github qbojj/sed))

(authors "Jakub Janeczko")

(maintainers "Jakub Janeczko")

(package
 (name sed)
 (synopsis "A simple sed clone with DFA regex matchers")
 (depends ocaml dune cmdliner))
