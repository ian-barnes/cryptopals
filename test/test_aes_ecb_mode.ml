open Lib

let test ~input ~key ~expected () =
  let key = Bytes.of_string key in
  let actual =
    input |> Base64.of_base64 |> Aes_ecb_mode.decrypt ~key |> Bytes.to_string
  in
  Alcotest.(check string) "Decrypt AES ECB mode with known key" expected actual

let tests =
  let ciphertext = File.read_all "7.txt" in
  let plaintext = File.read_all "7.decrypted.txt" in
  [ Alcotest.test_case "Challenge 7" `Quick
      (test ~input:ciphertext ~key:"YELLOW SUBMARINE" ~expected:plaintext) ]
