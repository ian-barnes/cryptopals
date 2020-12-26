let pad ~blocksize s =
  let len = Bytes.length s in
  let deficit = blocksize - (len mod blocksize) in
  let c = CCChar.of_int_exn deficit in
  Bytes.pad ~side:`Right ~c (len + deficit) s

let unpad ~blocksize s =
  let msg = "padding error" in
  let length = Bytes.length s in
  Assert.assert_with msg (length mod blocksize = 0);
  let last = Bytes.get s (length - 1) in
  let deficit = CCChar.to_int last in
  Assert.assert_with msg (0 < deficit && deficit <= blocksize);
  let suffix = CCString.make deficit last |> Bytes.of_string in
  Bytes.chop_suffix_exn ~msg ~suffix s

(* Note that this function should never be used in this form for actual
   decryption as it is vulnerable to padding oracle attacks by design. *)
