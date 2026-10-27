# iso4217

Validated [ISO 4217](https://www.iso.org/iso-4217-currency-codes.html) alphabetic
currency codes for OCaml.

An `Iso4217.t` can only hold a code that is in the official list, so a value of that
type is always a real currency code. The list is compiled into the library: there is
no data file to ship or load at run time.

```ocaml
match Iso4217.of_alpha_code "EUR" with
| Some eur -> print_endline (Iso4217.to_alpha_code eur)
| None -> print_endline "not an ISO 4217 code"
```

## API

```ocaml
type t

val of_alpha_code : string -> t option  (* exact match: "usd" and "BTC" are None *)
val to_alpha_code : t -> string

val equal : t -> t -> bool
val compare : t -> t -> int
val hash_fold_t : Hash.state -> t -> Hash.state
val hash : t -> int

val sexp_of_t : t -> Sexp.t            (* the code as an atom *)
val t_of_sexp : Sexp.t -> t            (* raises on codes not in the list *)
```

`compare`, `equal` and `hash` make `Iso4217.t` usable as a key in Base maps, sets and
hash tables. The library depends only on [Base](https://github.com/janestreet/base).

## Data

The codes come from `lib/list-one.xml`, the current-currencies list ("List One")
published by SIX Financial Information, the ISO 4217 maintenance agency. The bundled
copy is dated 2024-06-25 and has 179 codes, including fund codes (`USN`, `CLF`, …),
precious metals (`XAU`, `XAG`, …) and the other X codes (`XDR`, `XXX`, …).

At build time, `iso4217-gen` (in `bin/`) reads the XML and generates the sorted
array of codes that `of_alpha_code` searches.

To update the list, download the latest version and rebuild:

```sh
make download
dune build @runtest
```

Only alphabetic codes are exposed. Numeric codes, minor units and country names are in
the XML but not in the API.

## Building

```sh
dune build
dune test
```

Building needs [xmlm](https://erratique.ch/software/xmlm) for the generator; the
library itself does not depend on it at run time.

## License

ISC. See [LICENSE](LICENSE).
