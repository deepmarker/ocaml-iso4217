include module type of Iso4217_base
open Json_encoding

val info : t -> info list
val of_alpha_code : string -> t option
val to_alpha_code : t -> string
val info_encoding : info encoding
