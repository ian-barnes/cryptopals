open Lib

let test_encode ~input ~expected () =
  let actual = input |> Bytes.of_string |> Base64.encode in
  Alcotest.(check string) "encode" expected actual

let test_to_bytes ~input ~expected () =
  let actual = input |> Base64.decode |> Bytes.to_string in
  Alcotest.(check string) "to_bytes" expected actual

let tests =
  [ Alcotest.test_case "abc encode" `Quick
      (test_encode ~input:"abc" ~expected:"YWJj")
  ; Alcotest.test_case "abcd encode" `Quick
      (test_encode ~input:"abcd" ~expected:"YWJjZA==")
  ; Alcotest.test_case "abcde encode" `Quick
      (test_encode ~input:"abcde" ~expected:"YWJjZGU=")
  ; Alcotest.test_case "abc to_bytes" `Quick
      (test_to_bytes ~input:"YWJj" ~expected:"abc")
  ; Alcotest.test_case "abcd to_bytes" `Quick
      (test_to_bytes ~input:"YWJjZA==" ~expected:"abcd")
  ; Alcotest.test_case "abcde to_bytes" `Quick
      (test_to_bytes ~input:"YWJjZGU=" ~expected:"abcde") ]
