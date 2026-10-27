(lang dune 3.21)

(name iso4217)

(generate_opam_files true)

(source
 (github deepmarker/ocaml-iso4217))

(authors "Vincent Bernardoff")

(maintainers "Vincent Bernardoff")

(license ISC)

(documentation https://url/to/documentation)

(package
 (name iso4217)
 (synopsis "ISO4217 (currency codes)")
 (description "Validated ISO 4217 currency codes generated from the official XML list")
 (depends
  ocaml
  dune
  (base (>= v0.17.1))
  (ezxmlm :build)
 )
 (tags (iso4217 currency)))
