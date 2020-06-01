open Lib

let test ~first ~second ~expected () =
  let actual =
    Repeating_key_xor.hamming_distance (Bytes.of_string first)
      (Bytes.of_string second)
  in
  Alcotest.(check int) "Hamming distance" expected actual

let tests =
  [ Alcotest.test_case "a/a/0" `Quick (test ~first:"a" ~second:"a" ~expected:0)
  ; Alcotest.test_case "a/b/2" `Quick (test ~first:"a" ~second:"b" ~expected:2)
  ; Alcotest.test_case "a/c/1" `Quick (test ~first:"a" ~second:"c" ~expected:1)
  ; Alcotest.test_case "aa/bc/3" `Quick
      (test ~first:"aa" ~second:"bc" ~expected:3)
  ; Alcotest.test_case "Challenge 6, step 2" `Quick
      (test ~first:"this is a test" ~second:"wokka wokka!!!" ~expected:37) ]
