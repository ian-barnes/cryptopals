open Lib

let test_decrypt ~key ~nonce ~ciphertext ~expected () =
  let actual =
    ciphertext
    |> Base64.of_base64
    |> Ctr_mode.decrypt ~key ~nonce
    |> Bytes.to_printable_string
  in
  Alcotest.(check string) "CTR mode decryption" expected actual

let test_round_trip ~msg () =
  let expected = msg in
  let key = Aes.random_key () in
  let nonce = Bytes.random 8 in
  let actual =
    msg
    |> Bytes.of_string
    |> Ctr_mode.encrypt ~key ~nonce
    |> Ctr_mode.decrypt ~key ~nonce
    |> Bytes.to_printable_string
  in
  Alcotest.(check string) "CTR mode round trip" expected actual

let tests =
  [ Alcotest.test_case "Example" `Quick
      (test_decrypt
         ~key:(Bytes.of_string "YELLOW SUBMARINE")
         ~nonce:(Bytes.zeros ~length:8)
         ~ciphertext:
           "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
         ~expected:"Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby ")
  ; Alcotest.test_case "CTR mode round trip: Multiple of blocksize" `Quick
      (test_round_trip ~msg:"1234567890123456789012345689012")
  ; Alcotest.test_case "CTR mode round trip: One short" `Quick
      (test_round_trip ~msg:"123456789012345678901234568901")
  ; Alcotest.test_case "CTR mode round trip: One long" `Quick
      (test_round_trip ~msg:"12345678901234567890123456890123")
  ; Alcotest.test_case "CTR mode round trip: Empty" `Quick
      (test_round_trip ~msg:"") ]
