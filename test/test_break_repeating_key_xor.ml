open Lib

let test_key ~input ~expected () =
  let actual =
    input |> Base64.decode |> Repeating_key_xor.guess_key |> Bytes.to_string
  in
  Alcotest.(check string) "Break repeating key XOR" expected actual

let test_decrypt ~input ~expected () =
  let actual =
    input |> Base64.decode |> Repeating_key_xor.crack |> snd |> Bytes.to_string
  in
  Alcotest.(check string) "Crack and decrypt repeating key XOR" expected actual

let tests =
  let challenge = File.read_all "6.txt" in
  let target = File.read_all "6.decrypted.txt" in
  [ Alcotest.test_case "Challenge 6 get key" `Quick
      (test_key ~input:challenge ~expected:"Terminator X: Bring the noise")
  ; Alcotest.test_case "Challenge 6 decrypt" `Quick
      (test_decrypt ~input:challenge ~expected:target) ]
