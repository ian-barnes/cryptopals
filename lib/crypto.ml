module CharMap = CCMap.Make (struct
  type t = char

  let compare = compare
end)

let etaoin =
  [ (' ', 2000) (* rough guess *)
  ; ('E', 1249) (* remainder from Norvig *)
  ; ('T', 928)
  ; ('A', 804)
  ; ('O', 764)
  ; ('I', 757)
  ; ('N', 723)
  ; ('S', 651)
  ; ('R', 628)
  ; ('H', 505)
  ; ('L', 407)
  ; ('D', 382)
  ; ('C', 334)
  ; ('U', 273)
  ; ('M', 251)
  ; ('F', 240)
  ; ('P', 214)
  ; ('G', 187)
  ; ('W', 168)
  ; ('Y', 166)
  ; ('B', 148)
  ; ('V', 105)
  ; ('K', 54)
  ; ('X', 23)
  ; ('J', 16)
  ; ('Q', 12)
  ; ('Z', 9) ]
  |> CCList.map (fun (c, n) -> (CCChar.lowercase_ascii c, n))
  |> CharMap.of_list

let frequencies (s : Bytes.t) =
  let f = function
    | Some v -> Some (v + 1)
    | None -> Some 1
  in
  Bytes.fold (fun m c -> CharMap.update c f m) CharMap.empty s

let dot_product m m' =
  let prod _ vs =
    match vs with
    | `Left _
    | `Right _ ->
      None
    | `Both (a, b) -> Some (a * b)
  in
  let merged = CharMap.merge_safe ~f:prod m m' in
  CharMap.fold (fun _ v acc -> v + acc) merged 0

let score (s : Bytes.t) =
  dot_product (frequencies (Bytes.lowercase_ascii s)) etaoin

let crack_single_char_xor (s : Bytes.t) : char * Bytes.t =
  (* let s = Hex.of_hex_string s in *)
  let mask c =
    CCString.repeat (CCChar.to_string c) (Bytes.length s) |> Bytes.of_string
  in
  CharMap.empty
  |> CCList.fold_right
       (fun c m -> CharMap.add c (Bytes.xor s (mask c)) m)
       (CCList.map CCChar.of_int_exn (Util.range 0 255))
  |> CharMap.map (fun s -> (s, score s))
  |> CharMap.to_list
  |> CCList.sort (fun (_, (_, n)) (_, (_, n')) -> compare n' n)
  |> CCList.hd
  |> fun (key, (plaintext, _score)) -> (key, plaintext)

let decode_single_char_xor b =
  b |> crack_single_char_xor |> fun (_key, plaintext) -> plaintext

let detect_single_char_xor b =
  b
  |> CCList.map (fun line ->
         let plaintext = decode_single_char_xor line in
         (plaintext, score plaintext))
  |> CCList.sort (fun (_, score) (_, score') -> compare score' score)
  |> CCList.hd
  |> fst

module Repeating_key_xor = struct
  let encode ~(key : Bytes.t) (msg : Bytes.t) =
    let m = Bytes.length key in
    Assert.assert_with "key length must be positive" (m > 0);
    let n = Bytes.length msg in
    let key = Bytes.repeat key (1 + (n / m)) |> Bytes.take n in
    Bytes.xor key msg

  let decode = encode

  let hamming_weight (s : Bytes.t) =
    let weight n =
      let rec worker n count =
        match n with
        | 0 -> count
        | n -> worker (n land (n - 1)) (count + 1)
      in
      worker n 0
    in
    s
    |> Bytes.to_char_list
    |> CCList.map CCChar.to_int
    |> CCList.map weight
    |> CCList.fold_left (fun a b -> a + b) 0

  let hamming_distance (s : Bytes.t) (s' : Bytes.t) =
    Assert.assert_with "lengths must be equal" (Bytes.length s = Bytes.length s');
    Bytes.xor s s' |> hamming_weight

  (* For each KEYSIZE, take the first KEYSIZE worth of bytes, and the second
     KEYSIZE worth of bytes, and find the edit distance between them. Normalize
     this result by dividing by KEYSIZE. *)
  let key_length_score (s : Bytes.t) len =
    let rec worker start acc =
      if start + len > Bytes.length s then
        acc
      else
        worker (start + len) (Bytes.sub s start len :: acc)
    in
    let blocks = worker 0 [] in
    let pairs = CCList.diagonal blocks in
    let scores =
      CCList.map
        (fun (a, b) -> Float.of_int (hamming_distance a b) /. Float.of_int len)
        pairs
    in
    if CCList.length scores = 0 then
      8.0
    else
      CCList.fold_left (fun x y -> x +. y) 0.0 scores
      /. Float.of_int (CCList.length scores)

  let best_guess_key_length (s : Bytes.t) =
    let scores =
      Util.range 2 40
      |> CCList.map (fun length -> (length, key_length_score s length))
      |> CCList.sort (fun (_, score) (_, score') -> compare score score')
    in
    (* CCList.iter
       (fun (len, score) -> print_endline (Printf.sprintf "%d -> %f" len score))
       (scores |> CCList.take 10); *)
    scores |> CCList.hd |> fst

  let guess_key (b : Bytes.t) =
    (* Guess key length *)
    let n = best_guess_key_length b in
    (* Split ciphertext into bands by index mod key length *)
    let stripes = Util.stripes n b in
    (* Find the single character key for each band *)
    let keys =
      CCList.map (fun bs -> bs |> crack_single_char_xor |> fst) stripes
    in
    (* Assemble the characters to get the actual key *)
    keys |> Bytes.of_char_list

  let crack s =
    let key = guess_key s in
    (key, decode ~key s)
end

module Aes_ecb_mode = struct
  (* Assume 128 bit = 16 byte block size *)
  open Nocrypto.Cipher_block

  let encrypt ~key msg =
    Assert.assert_with "AES key length must be 128 bits" (Bytes.length key = 16);
    Assert.assert_with "AES msg bad length" (Bytes.length msg mod 16 = 0);
    let key =
      key |> Bytes.to_string |> Cstruct.of_string |> AES.ECB.of_secret
    in
    let msg = msg |> Bytes.to_string |> Cstruct.of_string in
    AES.ECB.encrypt ~key msg |> Cstruct.to_string |> Bytes.of_string

  let decrypt ~key msg =
    Assert.assert_with "AES key length must be 128 bits" (Bytes.length key = 16);
    Assert.assert_with "AES msg bad length" (Bytes.length msg mod 16 = 0);
    let key =
      key |> Bytes.to_string |> Cstruct.of_string |> AES.ECB.of_secret
    in
    let msg = msg |> Bytes.to_string |> Cstruct.of_string in
    AES.ECB.decrypt ~key msg |> Cstruct.to_string |> Bytes.of_string

  let detect (texts : Bytes.t list) : Bytes.t =
    texts
    |> CCList.map (fun s -> (s, Repeating_key_xor.key_length_score s 16))
    |> CCList.sort (fun (_, x) (_, x') -> compare x x')
    |> CCList.hd
    |> fst
end

module Pkcs7 = struct
  let pad ~blocksize s =
    let len = Bytes.length s in
    let deficit = blocksize - (len mod blocksize) in
    Printf.printf "blocksize = %d, len = %d, deficit = %d" blocksize len deficit;
    let c = CCChar.of_int_exn deficit in
    Bytes.pad ~side:`Right ~c (len + deficit) s
end
