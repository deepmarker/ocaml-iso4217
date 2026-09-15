include Iso4217_base
open Bin_prot.Std
open Base

type t = string [@@deriving bin_io, sexp, hash]

let of_alpha_code x =
  Array.binary_search ~compare:String.compare codes `First_equal_to x
  |> Option.map ~f:(Array.get codes)
;;

let to_alpha_code t = t
