open Lib

let () =
  let open Nocrypto.Cipher_block in
  let key = AES.ECB.of_secret (Cstruct.of_string "YELLOW SUBMARINE") in
  let ciphertext = File.read_all "data/7.txt" |> Crypto.Base64.of_base64 |> Bytes.to_string |> Cstruct.of_string in
  let plaintext = AES.ECB.decrypt ~key ciphertext |> Cstruct.to_string in
  print_endline ("plaintext = \n'" ^ plaintext ^ "'");

  File.write_all "foo.txt" plaintext
