open Lib

let test_to_base64 ~input ~expected () =
  let actual = input |> Bytes.of_string |> Base64.to_base64 in
  Alcotest.(check string) "to_base64" expected actual

let test_of_base64 ~input ~expected () =
  let actual = input |> Base64.of_base64 |> Bytes.to_string in
  Alcotest.(check string) "of_base64" expected actual

let tests =
  [ Alcotest.test_case "abc to_base64" `Quick
      (test_to_base64 ~input:"abc" ~expected:"YWJj")
  ; Alcotest.test_case "abcd to_base64" `Quick
      (test_to_base64 ~input:"abcd" ~expected:"YWJjZA==")
  ; Alcotest.test_case "abcde to_base64" `Quick
      (test_to_base64 ~input:"abcde" ~expected:"YWJjZGU=")
  ; Alcotest.test_case "abc of_base64" `Quick
      (test_of_base64 ~input:"YWJj" ~expected:"abc")
  ; Alcotest.test_case "abcd of_base64" `Quick
      (test_of_base64 ~input:"YWJjZA==" ~expected:"abcd")
  ; Alcotest.test_case "abcde of_base64" `Quick
      (test_of_base64 ~input:"YWJjZGU=" ~expected:"abcde") ]
