(* For now the cipher is hard-coded AES, but this could / should be modified to
   take block cipher argument. *)

let encrypt ~key ~iv msg =
  Assert.assert_with "key must be 16 bytes" (Bytes.length key = 16);
  Assert.assert_with "iv must be 16 bytes" (Bytes.length iv = 16);
  Assert.assert_with "bad msg length" (Bytes.length msg mod 16 = 0);
  let blocks = Blocks.of_bytes msg in
  let rec worker blocks last_block acc =
    match blocks with
    | [] -> CCList.rev acc
    | b :: bs ->
      let new_block = Aes_ecb_mode.encrypt ~key (Bytes.xor b last_block) in
      worker bs new_block (new_block :: acc)
  in
  worker blocks iv [] |> Blocks.to_bytes

let decrypt ~key ~iv msg =
  Assert.assert_with "key must be 16 bytes" (Bytes.length key = 16);
  Assert.assert_with "iv must be 16 bytes" (Bytes.length iv = 16);
  Assert.assert_with "bad msg length" (Bytes.length msg mod 16 = 0);
  let blocks = Blocks.of_bytes msg in
  let rec worker blocks last_block acc =
    match blocks with
    | [] -> CCList.rev acc
    | b :: bs ->
      let new_block = Bytes.xor (Aes_ecb_mode.decrypt ~key b) last_block in
      worker bs b (new_block :: acc)
  in
  worker blocks iv [] |> Blocks.to_bytes
