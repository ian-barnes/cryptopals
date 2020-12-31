open Lib.Ecb_decryption_harder

let test_decryption () =
  let expected = Lib.File.read_all "12.decrypted.txt" in
  let actual = Client.decrypt Server.oracle |> Lib.Bytes.to_string in
  Alcotest.(check string) "Decrypt AES/ECB" expected actual

let tests = [Alcotest.test_case "ECB decryption (harder)" `Quick test_decryption]
