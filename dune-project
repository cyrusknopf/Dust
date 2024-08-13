(lang dune 3.16)

(name dust)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Cyrus Knopf")

(maintainers "Cyrus Knopf")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name dust)
 (synopsis "OCaml advanced DS")
 (description "A longer description")
 (depends 
   ocaml 
   dune
   (alcotest :with-test)
 )
 (tags
  (topics "to describe" your project))
 )


; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
