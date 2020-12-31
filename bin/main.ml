open Lib
open Lib.Ecb_decryption_harder

let () =
  let result = Client.decrypt Server.oracle in
  Printf.printf "%s\n" (Bytes.to_string result)
