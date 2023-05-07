(lang dune 2.8)
(name saguaro)
(generate_opam_files true)
(source
 (github benmandrew/saguaro))
(authors "Ben Andrew")
(maintainers "Ben Andrew")
(license MIT)

(package
 (name saguaro)
 (synopsis "Propositional logic SAT solver using the DPLL method. Supported by a lexer, parser, and clause form converter")
 (description "Propositional logic SAT solver using the DPLL method. Supported by a lexer, parser, and clause form converter")
 (depends ocaml dune))
