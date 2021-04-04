open Lib

let () =
  let msg =
    "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
  in
  let key = Bytes.of_string "YELLOW SUBMARINE" in
  let nonce = Bytes.zeros ~length:8 in
  Printf.printf "\"%s\"\n"
    (msg
    |> Base64.of_base64
    |> Ctr_mode.decrypt ~key ~nonce
    |> Bytes.to_printable_string)
