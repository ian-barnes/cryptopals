let keystream_block ~(key : Bytes.t) ~(nonce : Bytes.t) (n : int) : Bytes.t =
  Assert.assert_with "counter must be in range 0..255" (n >= 0 && n < 256);
  let counter = CCChar.chr n in
  let msg = Bytes.(nonce || of_char counter || zeros ~length:7) in
  Assert.assert_with "wrong length" (Bytes.length msg = Aes.blocksize);
  Aes.encrypt_single_block ~key msg

let keystream ~(key : Bytes.t) ~(nonce : Bytes.t) (length : int) : Bytes.t =
  let rec worker (counter : int) (acc : Blocks.t) : Bytes.t =
    if counter * Aes.blocksize > length then
      acc |> CCList.rev |> Blocks.to_bytes |> Bytes.take length
    else
      let next_block = keystream_block ~key ~nonce counter in
      worker (counter + 1) (next_block :: acc)
  in
  worker 0 []

let encrypt
    ~(key : Bytes.t)
    ?(nonce : Bytes.t = Bytes.zeros ~length:8)
    (msg : Bytes.t) : Bytes.t =
  let keystream = keystream ~key ~nonce (Bytes.length msg) in
  Bytes.xor keystream msg

let decrypt = encrypt
