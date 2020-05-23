open Lib

let test_hex_to_base64 () =
  let test ~input ~expected =
    let actual = Crypto.hex_to_base64 input in
    Alcotest.(check string) "hex to Base64" expected actual
  in
  test
    ~input:
      "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    ~expected:"SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

module Hex = struct
  let test_of_hex ~input ~expected () =
    let actual = input |> Crypto.Hex.of_hex_string |> Crypto.Bytes.to_string in
    Alcotest.(check string) "of_hex_string" expected actual

  let test_to_hex ~input ~expected () =
    let actual = input |> Crypto.Bytes.of_string |> Crypto.Hex.to_hex_string in
    Alcotest.(check string) "to_hex_string" expected actual

  let tests =
    [
      Alcotest.test_case "test_of_hex" `Quick
        (test_of_hex
           ~input:
             ( "6162636465666768696a6b6c6d6e6f707172737475767778797a4142434445"
             ^ "464748494a4b4c4d4e4f505152535455565758595a30313233343536373839"
             )
           ~expected:
             "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");
      Alcotest.test_case "test_to_hex" `Quick
        (test_to_hex
           ~input:
             "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
           ~expected:
             ( "6162636465666768696a6b6c6d6e6f707172737475767778797a4142434445"
             ^ "464748494a4b4c4d4e4f505152535455565758595a30313233343536373839"
             ));
    ]
end

module Base64 = struct
  let test_to_base64 ~input ~expected () =
    let actual = input |> Crypto.Bytes.of_string |> Crypto.Base64.to_base64 in
    Alcotest.(check string) "to_base64" expected actual

  let test_of_base64 ~input ~expected () =
    let actual = input |> Crypto.Base64.of_base64 |> Crypto.Bytes.to_string in
    Alcotest.(check string) "of_base64" expected actual

  let tests =
    [
      Alcotest.test_case "abc to_base64" `Quick
        (test_to_base64 ~input:"abc" ~expected:"YWJj");
      Alcotest.test_case "abcd to_base64" `Quick
        (test_to_base64 ~input:"abcd" ~expected:"YWJjZA==");
      Alcotest.test_case "abcde to_base64" `Quick
        (test_to_base64 ~input:"abcde" ~expected:"YWJjZGU=");
      Alcotest.test_case "abc of_base64" `Quick
        (test_of_base64 ~input:"YWJj" ~expected:"abc");
      Alcotest.test_case "abcd of_base64" `Quick
        (test_of_base64 ~input:"YWJjZA==" ~expected:"abcd");
      Alcotest.test_case "abcde of_base64" `Quick
        (test_of_base64 ~input:"YWJjZGU=" ~expected:"abcde");
    ]
end

let test_xor () =
  let test ~first ~second ~expected =
    let first = Crypto.Hex.of_hex_string first in
    let second = Crypto.Hex.of_hex_string second in
    let actual = Crypto.Bytes.xor first second |> Crypto.Hex.to_hex_string in
    Alcotest.(check string) "Challenge 2" expected actual
  in
  test ~first:"1c0111001f010100061a024b53535009181c"
    ~second:"686974207468652062756c6c277320657965"
    ~expected:"746865206b696420646f6e277420706c6179"

let test_frequency_analysis () =
  let test ~input ~expected =
    let actual = Crypto.decode_single_char_xor input in
    Alcotest.(check string) "Challenge 3" expected actual
  in
  test
    ~input:
      "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    ~expected:"Cooking MC's like a pound of bacon"

let test_detect_single_char_xor () =
  let expected = "Now that the party is jumping?" in
  let actual = Crypto.detect_single_char_xor "4.txt" in
  Alcotest.(check string) "Challenge 4" expected actual

module Repeating_key_xor = struct
  let test ~input ~key ~expected () =
    let actual =
      Crypto.Bytes.repeating_key_xor
        ~key:(Crypto.Bytes.of_string key)
        (Crypto.Bytes.of_string input)
      |> Crypto.Hex.to_hex_string
    in
    Alcotest.(check string) "repeating_key_xor" expected actual

  let tests =
    [
      Alcotest.test_case "key longer than msg" `Quick
        (test ~input:"AA" ~key:"key" ~expected:"2a24");
      Alcotest.test_case "Challenge 5" `Quick
        (test
           ~input:
             "Burning 'em, if you ain't quick and nimble\n\
              I go crazy when I hear a cymbal" ~key:"ICE"
           ~expected:
             ( "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272"
             ^ "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
             ));
    ]
end

let () =
  Alcotest.run "cryptopals"
    [
      ("Hex encoding", Hex.tests);
      ("Base64 encoding", Base64.tests);
      ( "Challenge 1",
        [ Alcotest.test_case "Challenge 1" `Quick test_hex_to_base64 ] );
      ("Challenge 2", [ Alcotest.test_case "Challenge 2" `Quick test_xor ]);
      ( "Challenge 3",
        [ Alcotest.test_case "Challenge 3" `Quick test_frequency_analysis ] );
      ( "Challenge 4",
        [ Alcotest.test_case "Challenge 4" `Quick test_detect_single_char_xor ]
      );
      ("Repeating key XOR", Repeating_key_xor.tests);
    ]
