open Nocrypto.Cipher_block

(* Assume 128 bit = 16 byte block size *)

(* Should split this into an AES encrypt / decrypt single block function, and
   an ECB-mode encrypt / decrypt multiple blocks. Ideally, a block encryption
   should be a pipeline: msg |> padding |> block_mode |> single_block_fn. *)

let encrypt ~key msg =
  Assert.assert_with "AES key length must be 128 bits" (Bytes.length key = 16);
  Assert.assert_with "AES msg bad length" (Bytes.length msg mod 16 = 0);
  let key = key |> Bytes.to_string |> Cstruct.of_string |> AES.ECB.of_secret in
  let msg = msg |> Bytes.to_string |> Cstruct.of_string in
  AES.ECB.encrypt ~key msg |> Cstruct.to_string |> Bytes.of_string

let decrypt ~key msg =
  Assert.assert_with "AES key length must be 128 bits" (Bytes.length key = 16);
  Assert.assert_with "AES msg bad length" (Bytes.length msg mod 16 = 0);
  let key = key |> Bytes.to_string |> Cstruct.of_string |> AES.ECB.of_secret in
  let msg = msg |> Bytes.to_string |> Cstruct.of_string in
  AES.ECB.decrypt ~key msg |> Cstruct.to_string |> Bytes.of_string

let detect (texts : Bytes.t list) : Bytes.t =
  texts
  |> CCList.map (fun s -> (s, Repeating_key_xor.key_length_score s 16))
  |> CCList.sort (fun (_, x) (_, x') -> compare x x')
  |> CCList.hd
  |> fst
