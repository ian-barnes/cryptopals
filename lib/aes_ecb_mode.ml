let blocksize = Aes.blocksize

let encrypt ~key msg =
  Assert.assert_with "AES msg bad length" (Bytes.length msg mod blocksize = 0);
  msg
  |> Blocks.of_bytes
  |> CCList.map (Aes.encrypt_single_block ~key)
  |> Blocks.to_bytes

let decrypt ~key msg =
  Assert.assert_with "AES msg bad length" (Bytes.length msg mod blocksize = 0);
  msg
  |> Blocks.of_bytes
  |> CCList.map (Aes.decrypt_single_block ~key)
  |> Blocks.to_bytes

let detect (texts : Bytes.t list) : Bytes.t =
  texts
  |> CCList.map (fun s -> (s, Repeating_key_xor.key_length_score s blocksize))
  |> CCList.sort (fun (_, x) (_, x') -> compare x x')
  |> CCList.hd
  |> fst
