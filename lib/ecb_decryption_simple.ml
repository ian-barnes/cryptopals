(* Challenge 12 *)

module Server = struct
  (* Part 1: the server. The oracle takes arbitrary input from a client, appends
     the unknown string, PKCS#7 pads the result, encrypts it with an unknown key
     using AES/ECB, and returns the resulting ciphertext. *)

  let blocksize = Aes.blocksize

  let key = Aes.random_key ()

  let unknown =
    {|
      Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg
      aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq
      dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg
      YnkK
    |}
    |> Base64.of_base64

  let oracle data =
    data
    |> Bytes.append ~suffix:unknown
    |> Pkcs7_padding.pad ~blocksize
    |> Aes_ecb_mode.encrypt ~key
end

module Client = struct
  (* Part 2: the client. By sending carefully crafted inputs to the server,
     decrypt the unknown string. *)

  let zeros n = CCString.make n '\x00' |> Bytes.of_string

  let chars = Util.range 0 255 |> CCList.map Char.chr

  module BlockMap = CCMap.Make (Bytes)

  type dictionary = char BlockMap.t

  (* Step 1: Determine the block size and length of the unknown text *)

  let blocksize_and_length f =
    let a = Bytes.empty |> f |> Bytes.length in
    let (n, b) =
      Util.range 1 64
      |> CCList.map (fun n -> (n, zeros n |> f |> Bytes.length))
      |> CCList.find (fun (_, length) -> length > a)
    in
    let blocksize = b - a in
    let length = a - n in
    (blocksize, length)

  (* Step 2: Verify the block mode *)

  let block_mode = Ecb_cbc_detection_oracle.block_mode_detector

  (* Step 3: Decryption *)

  let decrypt f =
    Assert.assert_with "Only works for ECB mode"
      (block_mode f = Ecb_cbc_detection_oracle.Block_mode.ECB);

    let (blocksize, length) = blocksize_and_length f in

    let rec worker ~byte_num ~block_num ~msg ~pad =
      if byte_num = length then
        msg
      else
        (* Decrypt one additional byte *)
        let pad = Bytes.drop 1 pad in
        let table =
          CCList.fold_right
            (fun (c : char) (map : dictionary) ->
              let payload =
                pad
                |> Bytes.append ~suffix:msg
                |> Bytes.append ~suffix:(Bytes.of_char c)
              in
              let encrypted_block =
                payload
                |> f
                |> Blocks.of_bytes
                |> CCList.get_at_idx_exn block_num
              in
              BlockMap.add encrypted_block c map)
            chars BlockMap.empty
        in
        let target =
          pad |> f |> Blocks.of_bytes |> CCList.get_at_idx_exn block_num
        in
        let next_char =
          try BlockMap.find target table with
          | Not_found -> failwith "no match found in table!"
        in
        let msg = Bytes.append msg ~suffix:(Bytes.of_char next_char) in
        let (block_num, pad) =
          if Bytes.is_empty pad then
            (block_num + 1, zeros blocksize)
          else
            (block_num, pad)
        in
        let byte_num = byte_num + 1 in
        worker ~byte_num ~block_num ~msg ~pad
    in
    worker ~byte_num:0 ~block_num:0 ~msg:Bytes.empty ~pad:(zeros blocksize)
end
