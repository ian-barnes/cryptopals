open Lib

let test ~input ~expected () =
  let actual =
    input
    |> Bytes.of_string
    |> Crypto.Pkcs7.pad ~blocksize:16
    |> Bytes.to_string
  in
  Alcotest.(check string) "Correct PKCS#7 padding" expected actual

let tests =
  [ Alcotest.test_case "Four short" `Quick
      (test ~input:"YELLOW SUBMA" ~expected:"YELLOW SUBMA\x04\x04\x04\x04")
  ; Alcotest.test_case "Three short" `Quick
      (test ~input:"YELLOW SUBMAR" ~expected:"YELLOW SUBMAR\003\003\003")
  ; Alcotest.test_case "Two short" `Quick
      (test ~input:"YELLOW SUBMARI" ~expected:"YELLOW SUBMARI\002\002")
  ; Alcotest.test_case "One short" `Quick
      (test ~input:"YELLOW SUBMARIN" ~expected:"YELLOW SUBMARIN\001")
  ; Alcotest.test_case "Full block" `Quick
      (test ~input:"YELLOW SUBMARINE"
         ~expected:
           ( "YELLOW SUBMARINE"
           ^ "\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10"
           ))
  ; Alcotest.test_case "One long" `Quick
      (test ~input:"YELLOW SUBMARINES"
         ~expected:
           ( "YELLOW SUBMARINES"
           ^ "\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f" ))
  ]
