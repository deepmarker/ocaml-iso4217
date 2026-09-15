open Bin_prot.Std
open Base

type t [@@deriving bin_io, sexp, hash]

val of_alpha_code : string -> t option
val to_alpha_code : t -> string
