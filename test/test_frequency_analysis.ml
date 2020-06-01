open Lib

let test ~input ~expected () =
  let actual =
    input |> Hex.of_hex_string |> Single_char_xor.decode |> Bytes.to_string
  in
  Alcotest.(check string) "Challenge 3" expected actual

let tests =
  [ Alcotest.test_case "Challenge 3" `Quick
      (test
         ~input:
           "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
         ~expected:"Cooking MC's like a pound of bacon") ]
