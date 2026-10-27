open StdLabels
module String_set = Set.Make (String)

(* The text of every Ccy element. Entries without a currency, such as Antarctica's, have
   no Ccy element. *)
let codes input =
  let rec loop ~in_ccy codes =
    match Xmlm.input input with
    | `El_start ((_, "Ccy"), _) -> loop ~in_ccy:true codes
    | `Data code when in_ccy -> loop ~in_ccy (String_set.add (String.trim code) codes)
    | `El_end -> if Xmlm.eoi input then codes else loop ~in_ccy:false codes
    | `Dtd _ | `El_start _ | `Data _ -> loop ~in_ccy codes
  in
  loop ~in_ccy:false String_set.empty
;;

let generate ic oc =
  let codes = codes (Xmlm.make_input ~strip:true (`Channel ic)) in
  Format.fprintf oc "let codes = [|";
  String_set.iter (Format.fprintf oc "%S;") codes;
  Format.fprintf oc "|]@."
;;

let generate_file input output =
  In_channel.with_open_text input (fun ic ->
    Out_channel.with_open_text output (fun oc ->
      generate ic (Format.formatter_of_out_channel oc)))
;;

let () = generate_file Sys.argv.(1) Sys.argv.(2)
