open Lib

let test ~input ~key ~iv ~expected () =
  let actual = input |> Cbc_mode.decrypt ~key ~iv |> Bytes.to_string in
  Alcotest.(check string) "Decrypt CBC mode" expected actual

let tests =
  let key = "YELLOW SUBMARINE" |> Bytes.of_string in
  let iv = Bytes.zeros ~length:Aes.blocksize in
  let input = "10.txt" |> File.read_all |> Base64.decode in
  let expected = File.read_all "10.decrypted.txt" in
  [Alcotest.test_case "Challenge 10" `Quick (test ~input ~key ~iv ~expected)]
