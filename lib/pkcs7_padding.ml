let pad ~blocksize s =
  let len = Bytes.length s in
  let deficit = blocksize - (len mod blocksize) in
  Printf.printf "blocksize = %d, len = %d, deficit = %d" blocksize len deficit;
  let c = CCChar.of_int_exn deficit in
  Bytes.pad ~side:`Right ~c (len + deficit) s
