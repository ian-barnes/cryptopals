open Lib

let test ~input ~expected () =
  let actual =
    input
    |> CCList.map Hex.of_hex_string
    |> Crypto.Aes_ecb_mode.detect
    |> Bytes.to_string
  in
  let expected = expected |> Hex.of_hex_string |> Bytes.to_string in
  Alcotest.(check string) "Detect AES ECB mode" expected actual

let tests =
  let candidates = File.read_lines "8.txt" in
  let winner =
    {|
    d880619740a8a19b7840a8a31c810a3d08649af70dc06f4fd5d2d69c744cd283
    e2dd052f6b641dbf9d11b0348542bb5708649af70dc06f4fd5d2d69c744cd283
    9475c9dfdbc1d46597949d9c7e82bf5a08649af70dc06f4fd5d2d69c744cd283
    97a93eab8d6aecd566489154789a6b0308649af70dc06f4fd5d2d69c744cd283
    d403180c98c8f6db1f2a3f9c4040deb0ab51b29933f2c123c58386b06fba186a
    |}
  in
  [ Alcotest.test_case "Challenge 8" `Quick
      (test ~input:candidates ~expected:winner) ]
