open Base

let codes = [ "AED"; "EUR"; "USD"; "XAU"; "ZWG" ]

let test_round_trip code =
  let currency = Iso4217.of_alpha_code code |> Option.value_exn in
  assert (String.equal code (Iso4217.to_alpha_code currency));
  let decoded = Iso4217.t_of_sexp (Iso4217.sexp_of_t currency) in
  assert (String.equal code (Iso4217.to_alpha_code decoded))
;;

let test_rejects_unknown_sexp () =
  match Iso4217.t_of_sexp (Sexp.Atom "BTC") with
  | _ -> assert false
  | exception _ -> ()
;;

let () =
  List.iter codes ~f:test_round_trip;
  test_rejects_unknown_sexp ();
  assert (Option.is_none (Iso4217.of_alpha_code "BTC"));
  assert (Option.is_none (Iso4217.of_alpha_code "usd"))
;;
