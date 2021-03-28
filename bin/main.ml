open Lib

let data = Lib.File.read_lines "data/17.txt"

let () =
  data
  |> CCList.iter (fun data ->
         let msg = data |> Base64.of_base64 |> Bytes.to_string in
         let iv = Aes.random_key () in
         let (ciphertext, iv) = Vaudenay.Server.encrypt msg ~iv in
         let plaintext = Vaudenay.Client.crack ~ciphertext ~iv in
         Assert.assert_with "CBC padding oracle attack failed" (plaintext = msg);
         Printf.printf "\"%s\"\n" plaintext)
