(* Challenge 16 *)

module Server = struct
  (* The server we're attacking *)

  let key = Aes.random_key ()

  let print_blocks bytes =
    Printf.printf "%s\n" (bytes |> Blocks.of_bytes |> Blocks.to_printable_string)

  let quote s =
    let quote_char = function
      | '=' -> "%61"
      | ';' -> "%59"
      | c -> CCString.of_char c
    in
    CCString.flat_map quote_char s

  let encrypt ?(iv = Bytes.zeros ~length:Aes.blocksize) input =
    input
    |> quote
    |> (fun s ->
         "comment1=cooking%20MCs;userdata="
         ^ s
         ^ ";comment2=%20like%20a%20pound%20of%20bacon")
    |> Bytes.of_string
    |> Pkcs7_padding.pad
    |> Cbc_mode.encrypt ~key ~iv

  module StringMap = CCMap.Make (struct
    type t = string

    let compare = compare
  end)

  let parse data =
    data
    |> CCString.split ~by:";"
    |> CCList.map (fun s ->
           try CCString.Split.left_exn ~by:"=" s with
           | Not_found -> (s, ""))
    |> StringMap.of_list

  let decrypt ?(iv = Bytes.zeros ~length:Aes.blocksize) ciphertext =
    ciphertext
    |> Cbc_mode.decrypt ~key ~iv
    |> Pkcs7_padding.unpad
    |> Bytes.to_string

  let check_admin map =
    match StringMap.find_opt "admin" map with
    | Some s when s = "true" -> true
    | _ -> false

  let got_admin ?(iv = Bytes.zeros ~length:Aes.blocksize) ciphertext =
    ciphertext |> decrypt ~iv |> parse |> check_admin
end

module Client = struct
  (* Client flips bits in the ciphertext to get admin rights from the
     server. *)

  let naive_msg = "foobar;admin=true"

  let smart_msg = "foobarXadminYtrue"

  let flip_bits ciphertext =
    (* This is specific to smart_msg. The full plaintext looks like
       this:

       comment1=cooking
       %20MCs;userdata=
       foobarXadminYtru
       e;comment2=%20li
       ke%20a%20pound%2
       0of%20bacon\005\005\005\005\005

       so we just have to change the 'X' in block 2 to ';' and the
       'Y' to '='. *)
    let zero_block = Bytes.zeros ~length:Aes.blocksize in

    let big_x_at_bit_6 = Bytes.set 6 'X' zero_block in
    let semicolon_at_bit_6 = Bytes.set 6 ';' zero_block in
    let big_y_at_bit_12 = Bytes.set 12 'Y' zero_block in
    let equals_at_bit_12 = Bytes.set 12 '=' zero_block in

    (* If you XOR plaintext block 2 against all four of those blocks,
       that will achieve the desired result. Apply the same operations
       to Block 1 of the ciphertext. *)
    let tweak i block =
      if i = 1 then
        block
        |> Bytes.xor big_x_at_bit_6
        |> Bytes.xor semicolon_at_bit_6
        |> Bytes.xor big_y_at_bit_12
        |> Bytes.xor equals_at_bit_12
      else
        block
    in
    ciphertext |> Blocks.of_bytes |> CCList.mapi tweak |> Blocks.to_bytes

  let run () =
    (* This obvious injection attack shouldn't work *)
    Assert.assert_with "should fail"
      (naive_msg |> Server.encrypt |> Server.got_admin = false);

    (* The smart message shouldn't work without bit-flipping *)
    Assert.assert_with "should fail"
      (smart_msg |> Server.encrypt |> Server.got_admin = false);

    (* Instead we just make sure that what we want is easily obtainable
       by bit-flipping within a single block *)
    Assert.assert_with "should succeed"
      (smart_msg |> Server.encrypt |> flip_bits |> Server.got_admin = true);
    ()
end
