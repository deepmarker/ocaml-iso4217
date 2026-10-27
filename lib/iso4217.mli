open Base

(** ISO 4217 alphabetic currency codes, from the official list. *)
type t

val of_alpha_code : string -> t option
val to_alpha_code : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int

(** The alphabetic code as an atom. [t_of_sexp] rejects codes not in the list. *)
val sexp_of_t : t -> Sexp.t

val t_of_sexp : Sexp.t -> t
val hash_fold_t : Hash.state -> t -> Hash.state
val hash : t -> int
