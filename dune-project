(lang dune 3.16)

(name dust)

(generate_opam_files true)

(source
 (github cyrusknopf/dust))

(authors "Cyrus Knopf")

(maintainers "Cyrus Knopf")

(license LICENSE)

(documentation https://cyrusknopf.com)

(package
 (name dust)
 (synopsis "OCaml advanced DS")
 (description "Various datastructures, implemented in OCaml")
 (depends 
   ocaml 
   dune
 )
 (tags
  (topics "to describe" your project))
 )


; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
