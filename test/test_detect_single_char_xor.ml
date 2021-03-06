open Lib

let test_detect_single_char_xor ~input ~expected () =
  let actual =
    input |> CCList.map Hex.decode |> Single_char_xor.detect |> Bytes.to_string
  in
  Alcotest.(check string) "Challenge 4" expected actual

let tests =
  let input = "4.txt" |> File.read_lines in
  let expected = "Now that the party is jumping\n" in
  [ Alcotest.test_case "Challenge 4" `Quick
      (test_detect_single_char_xor ~input ~expected) ]
