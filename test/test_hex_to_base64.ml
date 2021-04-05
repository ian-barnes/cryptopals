open Lib

let test ~input ~expected () =
  let actual = input |> Hex.decode |> Base64.encode in
  Alcotest.(check string) "Hex to Base64" expected actual

let tests =
  [ Alcotest.test_case "Challenge 1" `Quick
      (test
         ~input:
           "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
         ~expected:
           "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  ]
