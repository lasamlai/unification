(lang dune 3.5)

(name unification)

(generate_opam_files true)

(source
  (github lasamlai/unification))

(authors "Samuel Li")

(maintainers "Samuel Li")

(license 0BSD)

(documentation https://url/to/documentation)

(package
  (name unification)
  (synopsis "Variables, unification and backtracking like in Prolog.")
  (description "Implementation of the unification algorithm with backtracking.")
  (depends (ocaml (>= 4.08.0)) dune)
  (tags
    (unification prolog backtracking "prolog variables")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
