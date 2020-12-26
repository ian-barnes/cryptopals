open Lib

let () =
  Printf.printf "\n\"%s\"\n"
    (Ecb_decryption_simple.Server.oracle
    |> Ecb_decryption_simple.Client.decrypt
    |> Bytes.to_string)
