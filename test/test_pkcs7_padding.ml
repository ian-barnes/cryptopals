open Lib

let test_pad ~input ~expected () =
  let actual =
    input |> Bytes.of_string |> Pkcs7_padding.pad |> Bytes.to_string
  in
  Alcotest.(check string) "Correct PKCS#7 padding" expected actual

let test_unpad_good ~input ~expected () =
  let actual =
    input |> Bytes.of_string |> Pkcs7_padding.unpad |> Bytes.to_string
  in
  Alcotest.(check string) "Correct padding" expected actual

let test_unpad_bad ~input () =
  let msg = "padding error" in
  let f () =
    let _ = input |> Bytes.of_string |> Pkcs7_padding.unpad in
    ()
  in
  Alcotest.check_raises "Bad padding" (Failure msg) f

let pad_tests =
  [ Alcotest.test_case "PKCS#7 pad: Four short" `Quick
      (test_pad ~input:"YELLOW SUBMA" ~expected:"YELLOW SUBMA\x04\x04\x04\x04")
  ; Alcotest.test_case "PKCS#7 pad: Three short" `Quick
      (test_pad ~input:"YELLOW SUBMAR" ~expected:"YELLOW SUBMAR\x03\x03\x03")
  ; Alcotest.test_case "PKCS#7 pad: Two short" `Quick
      (test_pad ~input:"YELLOW SUBMARI" ~expected:"YELLOW SUBMARI\x02\x02")
  ; Alcotest.test_case "PKCS#7 pad: One short" `Quick
      (test_pad ~input:"YELLOW SUBMARIN" ~expected:"YELLOW SUBMARIN\x01")
  ; Alcotest.test_case "PKCS#7 pad: Full block" `Quick
      (test_pad ~input:"YELLOW SUBMARINE"
         ~expected:
           ("YELLOW SUBMARINE"
           ^ "\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10"
           ))
  ; Alcotest.test_case "PKCS#7 pad: One long" `Quick
      (test_pad ~input:"YELLOW SUBMARINES"
         ~expected:
           ("YELLOW SUBMARINES"
           ^ "\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f")) ]

let unpad_tests =
  [ Alcotest.test_case "PKCS#7 unpad: Four short" `Quick
      (test_unpad_good ~input:"YELLOW SUBMA\x04\x04\x04\x04"
         ~expected:"YELLOW SUBMA")
  ; Alcotest.test_case "PKCS#7 unpad: Three short" `Quick
      (test_unpad_good ~input:"YELLOW SUBMAR\x03\x03\x03"
         ~expected:"YELLOW SUBMAR")
  ; Alcotest.test_case "PKCS#7 unpad: Two short" `Quick
      (test_unpad_good ~input:"YELLOW SUBMARI\x02\x02"
         ~expected:"YELLOW SUBMARI")
  ; Alcotest.test_case "PKCS#7 unpad: One short" `Quick
      (test_unpad_good ~input:"YELLOW SUBMARIN\x01" ~expected:"YELLOW SUBMARIN")
  ; Alcotest.test_case "PKCS#7 unpad: Full block" `Quick
      (test_unpad_good
         ~input:
           ("YELLOW SUBMARINE"
           ^ "\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10\x10"
           )
         ~expected:"YELLOW SUBMARINE")
  ; Alcotest.test_case "PKCS#7 unpad: One long" `Quick
      (test_unpad_good
         ~input:
           ("YELLOW SUBMARINES"
           ^ "\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f\x0f")
         ~expected:"YELLOW SUBMARINES")
  ; Alcotest.test_case "PKCS#7 unpad: Wrong padding value" `Quick
      (test_unpad_bad ~input:"YELLOW SUBMA\x05\x05\x05\x05")
  ; Alcotest.test_case "PKCS#7 unpad: Bad block length" `Quick
      (test_unpad_bad ~input:"YELLOW SUBMA")
  ; Alcotest.test_case "PKCS#7 unpad: Illegal padding value" `Quick
      (test_unpad_bad ~input:"YELLOW SUBMA\x00\x00\x00\x00")
  ; Alcotest.test_case "Challenge 15 example 1" `Quick
      (test_unpad_good ~input:"ICE ICE BABY\x04\x04\x04\x04"
         ~expected:"ICE ICE BABY")
  ; Alcotest.test_case "Challenge 15 example 2" `Quick
      (test_unpad_bad ~input:"ICE ICE BABY\x05\x05\x05\x05")
  ; Alcotest.test_case "Challenge 15 example 3" `Quick
      (test_unpad_bad ~input:"ICE ICE BABY\x01\x02\x03\x04") ]
