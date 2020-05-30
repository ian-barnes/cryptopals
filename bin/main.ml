open Lib

let () =
  let do_it n =
    Printf.printf " \"%s\"\n"
      ( "YELLOW SUBMARINE"
      |> Bytes.of_string
      |> Crypto.Pkcs7.pad ~blocksize:n
      |> Bytes.to_string
      |> CCString.escaped )
  in
  CCList.iter do_it [20; 19; 18; 17; 16]
