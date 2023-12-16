(lang dune 3.12)

(name mooc)

(generate_opam_files true)

(source
 (github qiuzhanghua/mooc))

(authors "qiuzhanghua")

(maintainers "qiuzhanghua")

(license APACHE-2.0)

(documentation https://url/to/documentation)

(package
 (name mooc)
 (synopsis "Learn ocaml")
 (description "Learn OCaml from ocaml-sf.org")
 (depends ocaml dune)
 (tags
  (leanr OCaml mooc)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
