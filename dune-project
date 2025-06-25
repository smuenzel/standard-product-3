(lang dune 3.18)

(name standard_product_3)

(generate_opam_files true)

(source
 (github smuenzel/standard-product-3))

(authors "Stefan Muenzel <source@s.muenzel.net>")

(maintainers "Stefan Muenzel <source@s.muenzel.net>")

(license MPL-2.0)

(documentation https://url/to/documentation)

(package
 (name standard_product_3)
 (synopsis "A parser for the SP3 orbit format")
 (description "A parser for the SP3 orbit format")
 (depends ocaml (fast_bitvector (>= 0.1.2.2)) angstrom core_unix core bigdecimal float_option decompress ppx_jane )
 (tags
  (sp3 orbit gnss)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
