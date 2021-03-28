open Nocrypto.Cipher_block

let blocksize = 16

(* Challenge 11, part 1 *)
let random_key () = Bytes.random blocksize

let encrypt_single_block ~key msg =
  Assert.assert_with "key length must be 128 bits" (Bytes.length key = blocksize);
  Assert.assert_with "msg length must be 128 bits" (Bytes.length msg = blocksize);
  let key = key |> Bytes.to_string |> Cstruct.of_string |> AES.ECB.of_secret in
  let msg = msg |> Bytes.to_string |> Cstruct.of_string in
  AES.ECB.encrypt ~key msg |> Cstruct.to_string |> Bytes.of_string

let decrypt_single_block ~key msg =
  Assert.assert_with "key length must be 128 bits" (Bytes.length key = blocksize);
  Assert.assert_with "msg length must be 128 bits" (Bytes.length msg = blocksize);
  let key = key |> Bytes.to_string |> Cstruct.of_string |> AES.ECB.of_secret in
  let msg = msg |> Bytes.to_string |> Cstruct.of_string in
  AES.ECB.decrypt ~key msg |> Cstruct.to_string |> Bytes.of_string
