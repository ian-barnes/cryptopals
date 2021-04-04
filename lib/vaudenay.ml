module Server = struct
  let key = Aes.random_key ()

  let encrypt ?(iv = Bytes.zeros ~length:Aes.blocksize) msg =
    let ciphertext =
      msg |> Bytes.of_string |> Pkcs7_padding.pad |> Cbc_mode.encrypt ~key ~iv
    in
    (ciphertext, iv)

  let decrypt ?(iv = Bytes.zeros ~length:Aes.blocksize) ciphertext =
    ciphertext
    |> Cbc_mode.decrypt ~key ~iv
    |> Pkcs7_padding.unpad
    |> Bytes.to_string

  let oracle ?(iv = Bytes.zeros ~length:Aes.blocksize) ciphertext =
    try
      let _ = decrypt ~iv ciphertext in
      true
    with
    | Failure _ -> false
end

module Client = struct
  let blocksize = Aes.blocksize

  let chars = Util.Int.range 0 255 |> CCList.map Char.chr

  let force_one_byte ~(c1 : Bytes.t) ~(c2 : Bytes.t) ~(acc : Bytes.t) : char =
    (* Given two consecutive blocks of ciphertext and a string containing a
       partial solution, i.e. the last however many characters of the
       corresponding plaintext, use the padding oracle to find the next
       character of the plaintext *)
    let length = Bytes.length acc in
    let pad = length + 1 in
    (* the padding byte value we're looking for *)
    let index = blocksize - pad in
    (* the index of the byte we're forcing *)
    let padding =
      Bytes.repeat (pad |> CCChar.of_int_exn |> Bytes.of_char) length
    in
    let c1_tail = Bytes.drop (blocksize - length) c1 in
    let c1p = Bytes.(take (index + 1) c1 || xor acc c1_tail |> xor padding) in
    let results =
      chars
      |> CCList.map (fun c -> (c, c1p |> Bytes.set index c))
      |> CCList.filter (fun (_, c1p) -> Server.oracle ~iv:c1p c2)
      |> CCList.map (fun (c, _) ->
             Util.Char.xor c (CCChar.chr pad)
             |> Util.Char.xor (c1 |> Bytes.get index))
      |> CCList.filter (fun c -> length > 0 || c != CCChar.chr pad)
    in
    Assert.assert_with "multiple hits!" (CCList.length results <= 1);
    Assert.assert_with "no hits!" (CCList.length results >= 1);
    let result = CCList.hd results in
    result

  let crack ~(ciphertext : Bytes.t) ~(iv : Bytes.t) : string =
    ciphertext
    |> Bytes.prepend ~prefix:iv
    |> Blocks.of_bytes
    |> Util.List.to_pairs
    |> CCList.map (fun (c1, c2) ->
           let rec worker (acc : Bytes.t) : Bytes.t =
             if Bytes.length acc = blocksize then
               acc
             else
               worker
                 (Bytes.prepend
                    ~prefix:(Bytes.of_char (force_one_byte ~c1 ~c2 ~acc))
                    acc)
           in
           worker Bytes.empty)
    |> Bytes.concat
    |> Pkcs7_padding.unpad
    |> Bytes.to_string
end
