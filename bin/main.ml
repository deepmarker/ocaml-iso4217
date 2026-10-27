open StdLabels
module String_set = Set.Make (String)

let add_code codes _attrs nodes =
  let open Ezxmlm in
  if has_member "Ccy" nodes
  then member "Ccy" nodes |> data_to_string |> Fun.flip String_set.add codes
  else codes
;;

let generate ic oc =
  let _dtd, nodes = Ezxmlm.from_channel ic in
  let codes = ref String_set.empty in
  Ezxmlm.filter_iter
    ~tag:"CcyNtry"
    ~f:(fun attrs nodes -> codes := add_code !codes attrs nodes)
    nodes;
  Format.fprintf oc "let codes = [|";
  String_set.iter (Format.fprintf oc "%S;") !codes;
  Format.fprintf oc "|]@."
;;

let generate_file input output =
  In_channel.with_open_text input (fun ic ->
    Out_channel.with_open_text output (fun oc ->
      generate ic (Format.formatter_of_out_channel oc)))
;;

let () = generate_file Sys.argv.(1) Sys.argv.(2)
