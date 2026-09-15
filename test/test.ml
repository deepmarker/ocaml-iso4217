open Base

let codes = [ "AED"; "EUR"; "USD"; "XAU"; "ZWG" ]

let test_round_trip code =
  let currency = Iso4217.of_alpha_code code |> Option.value_exn in
  assert (String.equal code (Iso4217.to_alpha_code currency));
  let decoded = Iso4217.t_of_sexp (Iso4217.sexp_of_t currency) in
  assert (String.equal code (Iso4217.to_alpha_code decoded))
;;

let test_bin_io_round_trip () =
  let usd = Iso4217.of_alpha_code "USD" |> Option.value_exn in
  let encoded = Bin_prot.Utils.bin_dump Iso4217.bin_writer_t usd in
  let pos_ref = ref 0 in
  let decoded = Iso4217.bin_read_t encoded ~pos_ref in
  assert (String.equal "USD" (Iso4217.to_alpha_code decoded))
;;

let () =
  List.iter codes ~f:test_round_trip;
  test_bin_io_round_trip ();
  assert (Option.is_none (Iso4217.of_alpha_code "BTC"));
  assert (Option.is_none (Iso4217.of_alpha_code "usd"))
;;
