(* For now the cipher is hard-coded AES, but this could / should be modified to
   take block cipher argument. *)

let blocksize = Aes.blocksize

let encrypt ~key ~iv msg =
  Assert.assert_with "key must be 16 bytes" (Bytes.length key = blocksize);
  Assert.assert_with "iv must be 16 bytes" (Bytes.length iv = blocksize);
  Assert.assert_with "bad msg length" (Bytes.length msg mod blocksize = 0);
  let blocks = Blocks.of_bytes msg in
  let rec worker blocks last_block acc =
    match blocks with
    | [] -> CCList.rev acc
    | b :: bs ->
      let new_block = Aes.encrypt_single_block ~key (Bytes.xor b last_block) in
      worker bs new_block (new_block :: acc)
  in
  worker blocks iv [] |> Blocks.to_bytes

let decrypt ~key ~iv msg =
  Assert.assert_with "key must be 16 bytes" (Bytes.length key = blocksize);
  Assert.assert_with "iv must be 16 bytes" (Bytes.length iv = blocksize);
  Assert.assert_with "bad msg length" (Bytes.length msg mod blocksize = 0);
  let blocks = Blocks.of_bytes msg in
  let rec worker blocks last_block acc =
    match blocks with
    | [] -> CCList.rev acc
    | b :: bs ->
      let new_block = Bytes.xor (Aes.decrypt_single_block ~key b) last_block in
      worker bs b (new_block :: acc)
  in
  worker blocks iv [] |> Blocks.to_bytes
