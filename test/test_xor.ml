open Lib

let test ~first ~second ~expected () =
  let first = Hex.decode first in
  let second = Hex.decode second in
  let actual = Bytes.xor first second |> Hex.encode in
  Alcotest.(check string) "Challenge 2" expected actual

let tests =
  [ Alcotest.test_case "Challenge 2" `Quick
      (test ~first:"1c0111001f010100061a024b53535009181c"
         ~second:"686974207468652062756c6c277320657965"
         ~expected:"746865206b696420646f6e277420706c6179") ]
