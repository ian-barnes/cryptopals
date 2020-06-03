open Lib

let test_of_hex ~input ~expected () =
  let actual = input |> Hex.of_hex_string |> Bytes.to_string in
  Alcotest.(check string) "of_hex_string" expected actual

let test_to_hex ~input ~expected () =
  let actual = input |> Bytes.of_string |> Hex.to_hex_string in
  Alcotest.(check string) "to_hex_string" expected actual

let tests =
  [ Alcotest.test_case "test_of_hex" `Quick
      (test_of_hex
         ~input:
           ("6162636465666768696a6b6c6d6e6f707172737475767778797a4142434445"
           ^ "464748494a4b4c4d4e4f505152535455565758595a30313233343536373839")
         ~expected:
           "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
  ; Alcotest.test_case "test_to_hex" `Quick
      (test_to_hex
         ~input:"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
         ~expected:
           ("6162636465666768696a6b6c6d6e6f707172737475767778797a4142434445"
           ^ "464748494a4b4c4d4e4f505152535455565758595a30313233343536373839"))
  ]
