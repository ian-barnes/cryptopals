open Lib.Ecb_decryption_simple

let test_decryption () =
  let expected = Lib.File.read_all "12.decrypted.txt" in
  let actual = Client.decrypt Server.oracle |> Lib.Bytes.to_string in
  Alcotest.(check string) "Decrypt AES/ECB" expected actual

let tests = [Alcotest.test_case "ECB decryption" `Quick test_decryption]
