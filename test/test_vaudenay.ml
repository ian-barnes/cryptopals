open Lib

let test ~input ~expected () =
  let (ciphertext, iv) = input in
  let actual = Vaudenay.Client.crack ~ciphertext ~iv in
  Alcotest.(check string) "Crack CBC mode" expected actual

let tests =
  Lib.File.read_lines "17.txt"
  |> CCList.map Base64.of_base64
  |> CCList.map Bytes.to_string
  |> CCList.mapi (fun num msg ->
         let iv = Aes.random_key () in
         let input = Vaudenay.Server.encrypt ~iv msg in
         Alcotest.test_case
           (Printf.sprintf "Sample %d" num)
           `Quick
           (test ~input ~expected:msg))
