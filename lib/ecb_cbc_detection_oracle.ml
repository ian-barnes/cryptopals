(* Challenge 11 *)

(* Nocrypto_entropy_unix.initialize ();
   let cs = Nocrypto.Rng.generate 16 in
   let s = cs |> Cstruct.to_string |> Bytes.of_string |> Hex.to_hex_string in
   print_endline s; *)

let _ = Random.self_init ()

let random_bytes length =
  let rec worker n acc =
    match n with
    | 0 -> acc |> CCList.rev_map CCChar.of_int_exn |> Bytes.of_char_list
    | n -> worker (n - 1) (Random.int 256 :: acc)
  in
  worker length []

(* Part 1: A function to generate a random AES key *)

let random_aes_key () = random_bytes 16

module Block_mode = struct
  type t =
    | ECB
    | CBC

  let to_string = function
    | ECB -> "ECB"
    | CBC -> "CBC"
end

(* Part 2: A function that encrypts data under a random key, using a block mode
   randomly chosen from ECB or CBC, and adding 5-10 random bytes both before and
   after the given plaintext input data. *)

let encryption_oracle_helper mode data =
  let key = random_aes_key () in
  let iv = random_aes_key () in
  let prefix_length = 5 + Random.int 5 in
  let prefix = random_bytes prefix_length in
  let suffix_length = 5 + Random.int 5 in
  let suffix = random_bytes suffix_length in
  let cipher =
    match mode with
    | Block_mode.ECB -> Aes_ecb_mode.encrypt ~key
    | CBC -> Cbc_mode.encrypt ~key ~iv
  in
  data
  |> Bytes.prepend ~prefix
  |> Bytes.append ~suffix
  |> Pkcs7_padding.pad ~blocksize:16
  |> cipher

let encryption_oracle data =
  let mode =
    if Random.bool () then
      Block_mode.ECB
    else
      Block_mode.CBC
  in
  encryption_oracle_helper mode data

(* Part 3: Block mode detector: a function that given a block cipher function as
   input, determines whether it is using ECB mode or CBC mode. *)

let block_mode_detector (cipher : Bytes.t -> Bytes.t) : Block_mode.t =
  let data = String.make 1024 '\x00' |> Bytes.of_string in
  let ciphertext = data |> cipher in
  let score = Repeating_key_xor.key_length_score ciphertext 16 in
  if score > 3.1 then
    Block_mode.CBC
  else
    Block_mode.ECB

(* This is a chosen plaintext attack. Our way of detecting ECB mode is that
   identical blocks will give identical ciphertext, so we want to be sure to
   generate some identical blocks. Sending 64 bits of all 0's doesn't seem to be
   sufficient, but with 128 bits, even with the random extra bytes, there are
   enough identical blocks that there is no doubt. The highest score for ECB is
   around 2.5, while the lowest for CBC is about 3.7. Increase the input length
   and the gap gets wider, but in several runs of a million, the 128-bit version
   was never wrong. The threshold of 3.1 is halfway between the ECB max and the
   CBC min. *)
