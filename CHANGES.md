# Changes

## Unreleased

- Depend on Base only: drop the `bin_io` derivation and the ppx_jane and bin_prot
  dependencies. `equal`, `compare`, `hash` and the sexp converters are written by hand.
- `t_of_sexp` rejects codes that are not in the ISO 4217 list, instead of accepting any
  string.
- Represent codes as opaque strings backed by a sorted array generated from the XML,
  instead of a large generated variant type, which was slow to compile.
- Remove the `info` metadata API (currency name, country, minor units, numeric code,
  fund flag) and its JSON encoding, and with them the json-data-encoding dependency.
- Format code and dune files with ocamlformat (janestreet profile).

## 2026-02-27

- Use json-data-encoding instead of ocplib-json-typed.

## 2024-12-01

- Initial version: currencies generated at build time from the SIX "List One" XML,
  with `info` metadata and a JSON encoding.
