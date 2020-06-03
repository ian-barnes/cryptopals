let pad ~blocksize s =
  let len = Bytes.length s in
  let deficit = blocksize - (len mod blocksize) in
  let c = CCChar.of_int_exn deficit in
  Bytes.pad ~side:`Right ~c (len + deficit) s
