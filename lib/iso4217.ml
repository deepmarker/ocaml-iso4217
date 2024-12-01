include Iso4217_base
open Base
open Json_encoding

let descrs = Variants.descriptions |> List.map ~f:fst |> Array.of_list

let of_alpha_code x =
  Array.binary_search ~compare:String.compare descrs `First_equal_to x
  |> Option.map ~f:Stdlib.Obj.magic
;;

let info_encoding =
  obj6
    (req "name" string)
    (req "alpha_code" string)
    (req "country" string)
    (req "decimals" int)
    (req "numeric_code" int)
    (req "is_fund" bool)
;;

let info_encoding =
  conv
    (fun { name; alpha_code; country; decimals; numeric_code; is_fund } ->
      name, alpha_code, country, decimals, numeric_code, is_fund)
    (fun (name, alpha_code, country, decimals, numeric_code, is_fund) ->
      { name; alpha_code; country; decimals; numeric_code; is_fund })
    info_encoding
;;

let info t =
  let nm = Variants.to_name t in
  List.fold_left infos ~init:[] ~f:(fun a x ->
    if String.equal nm x.alpha_code then x :: a else a)
;;

let to_alpha_code = Variants.to_name
