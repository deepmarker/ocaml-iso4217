include Iso4217_base
open Base

type t = string

let of_alpha_code x =
  Array.binary_search ~compare:String.compare codes `First_equal_to x
  |> Option.map ~f:(Array.get codes)
;;

let to_alpha_code t = t
let equal = String.equal
let compare = String.compare
let sexp_of_t = String.sexp_of_t

let t_of_sexp sexp =
  let code = String.t_of_sexp sexp in
  match of_alpha_code code with
  | Some t -> t
  | None -> Sexplib0.Sexp_conv.of_sexp_error "unknown ISO 4217 currency code" sexp
;;

let hash_fold_t = String.hash_fold_t
let hash = String.hash
