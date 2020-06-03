let test ~input ~key ~expected () =
  let actual =
    Lib.Repeating_key_xor.encode ~key:(Lib.Bytes.of_string key)
      (Lib.Bytes.of_string input)
    |> Lib.Hex.to_hex_string
  in
  Alcotest.(check string) "Repeating key XOR" expected actual

let tests =
  [ Alcotest.test_case "key longer than msg" `Quick
      (test ~input:"AA" ~key:"key" ~expected:"2a24")
  ; Alcotest.test_case "Challenge 5" `Quick
      (test
         ~input:
           "Burning 'em, if you ain't quick and nimble\n\
            I go crazy when I hear a cymbal"
         ~key:"ICE"
         ~expected:
           ("0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272"
           ^ "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
           )) ]
