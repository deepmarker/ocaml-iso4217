open StdLabels

type t =
  { name : string
  ; code : string
  ; country : string
  ; decimals : int
  ; number : int
  ; is_fund : bool
  }

let info_record =
  {|type info = {
name: string;
alpha_code: string;
country: string;
decimals: int;
numeric_code: int;
is_fund: bool;
} [@@deriving fields,bin_io,sexp,compare,hash]|}
;;

let genRecord ({ name; code; country; decimals; number; is_fund } : t) =
  let is_fund = Pla.bool is_fund in
  let country = String.escaped country in
  [%pla
    {|{name="<#name#s>";
alpha_code="<#code#s>";
country="<#country#s>";
decimals=(<#decimals#i>);
numeric_code=<#number#i>;
is_fund=<#is_fund#>}|}]
;;

let record_of_xml tbl _attrs nodes =
  let open Ezxmlm in
  match has_member "Ccy" nodes with
  | false -> ()
  | true ->
    let country = member "CtryNm" nodes |> data_to_string in
    let attrs, name = member_with_attr "CcyNm" nodes in
    let code = member "Ccy" nodes |> data_to_string in
    let decimals =
      member "CcyMnrUnts" nodes
      |> data_to_string
      |> function
      | "N.A." -> -1
      | n -> int_of_string n
    in
    let number = member "CcyNbr" nodes |> data_to_string |> int_of_string in
    let t =
      { name = data_to_string name
      ; code
      ; country
      ; decimals
      ; number
      ; is_fund = List.length attrs > 0
      }
    in
    Hashtbl.add tbl code t
;;

module StringMap = Map.Make (String)

let withJSON ic oc =
  let _dtd, nodes = Ezxmlm.from_channel ic in
  let tbl = Hashtbl.create 13 in
  Ezxmlm.filter_iter ~tag:"CcyNtry" ~f:(record_of_xml tbl) nodes;
  let ccys = Hashtbl.fold StringMap.add_to_list tbl StringMap.empty in
  let ccys = StringMap.bindings ccys in
  let open Pla in
  let typeT =
    string "type t = "
    ++ map_sep (string "|") string (List.map ~f:fst ccys)
    ++ string "[@@deriving variants,bin_io,sexp,compare,hash]"
  in
  let infos =
    string "let infos = "
    ++ string "["
    ++ map_sep semi genRecord List.(map ~f:snd ccys |> flatten)
    ++ string "]"
  in
  write
    oc
    (string "open Bin_prot.Std"
     ++ newline
     ++ string "open Base"
     ++ newline
     ++ typeT
     ++ newline
     ++ string info_record
     ++ newline
     ++ infos
     ++ newline)
;;

let () = In_channel.with_open_text Sys.argv.(1) (fun ic -> withJSON ic Sys.argv.(2))
