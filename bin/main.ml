open Lib
open Lib.Pkcs7_padding

let () =
  let blocksize = 16 in

  let a = "ICE ICE BABY\x04\x04\x04\x04" in
  let b = "ICE ICE BABY\x05\x05\x05\x05" in
  let c = "ICE ICE BABY\x01\x02\x03\x04" in

  let try_unpad s =
    try s |> Bytes.of_string |> unpad ~blocksize |> Bytes.to_string
    with Failure msg -> msg
  in

  let aa = try_unpad a in
  let bb = try_unpad b in
  let cc = try_unpad c in

  Printf.printf "aa = %s\n" aa;
  Printf.printf "bb = %s\n" bb;
  Printf.printf "cc = %s\n" cc
