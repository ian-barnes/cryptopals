module Hex = struct
  module Digit = struct
    type t = char

    let to_int c =
      match CCChar.lowercase_ascii c with
      | '0' -> 0
      | '1' -> 1
      | '2' -> 2
      | '3' -> 3
      | '4' -> 4
      | '5' -> 5
      | '6' -> 6
      | '7' -> 7
      | '8' -> 8
      | '9' -> 9
      | 'a' -> 10
      | 'b' -> 11
      | 'c' -> 12
      | 'd' -> 13
      | 'e' -> 14
      | 'f' -> 15
      | _ -> failwith "Hex.to_int: Invalid hex digit"

    let of_int n =
      match n with
      | 0 -> '0'
      | 1 -> '1'
      | 2 -> '2'
      | 3 -> '3'
      | 4 -> '4'
      | 5 -> '5'
      | 6 -> '6'
      | 7 -> '7'
      | 8 -> '8'
      | 9 -> '9'
      | 10 -> 'a'
      | 11 -> 'b'
      | 12 -> 'c'
      | 13 -> 'd'
      | 14 -> 'e'
      | 15 -> 'f'
      | _ -> failwith ("Hex.of_int: Invalid hex digit " ^ string_of_int n)
  end

  let int_of_hex_byte a b =
    let a = Digit.to_int a in
    let b = Digit.to_int b in
    (16 * a) + b

  let of_hex_string s =
    let s = Util.remove_whitespace s in
    let chars = CCString.to_list s in
    let rec worker cs acc =
      match cs with
      | a :: b :: tail -> worker tail (int_of_hex_byte a b :: acc)
      | [] -> Bytes.of_char_list (CCList.rev_map CCChar.of_int_exn acc)
      | _ -> failwith "Number of characters must be even"
    in
    worker chars []

  let to_hex_pair c =
    let n = CCChar.to_int c in
    (n lsr 4, n land 0xf)

  let to_hex_string s =
    let rec flattenpairs = function
      | [] -> []
      | (a, b) :: tail -> a :: b :: flattenpairs tail
    in
    s
    |> Bytes.to_char_list
    |> CCList.map to_hex_pair
    |> flattenpairs
    |> CCList.map Digit.of_int
    |> CCString.of_list
end

module Base64 = struct
  module Char = struct
    type t = char

    let of_int n =
      match n with
      | 0 -> 'A'
      | 1 -> 'B'
      | 2 -> 'C'
      | 3 -> 'D'
      | 4 -> 'E'
      | 5 -> 'F'
      | 6 -> 'G'
      | 7 -> 'H'
      | 8 -> 'I'
      | 9 -> 'J'
      | 10 -> 'K'
      | 11 -> 'L'
      | 12 -> 'M'
      | 13 -> 'N'
      | 14 -> 'O'
      | 15 -> 'P'
      | 16 -> 'Q'
      | 17 -> 'R'
      | 18 -> 'S'
      | 19 -> 'T'
      | 20 -> 'U'
      | 21 -> 'V'
      | 22 -> 'W'
      | 23 -> 'X'
      | 24 -> 'Y'
      | 25 -> 'Z'
      | 26 -> 'a'
      | 27 -> 'b'
      | 28 -> 'c'
      | 29 -> 'd'
      | 30 -> 'e'
      | 31 -> 'f'
      | 32 -> 'g'
      | 33 -> 'h'
      | 34 -> 'i'
      | 35 -> 'j'
      | 36 -> 'k'
      | 37 -> 'l'
      | 38 -> 'm'
      | 39 -> 'n'
      | 40 -> 'o'
      | 41 -> 'p'
      | 42 -> 'q'
      | 43 -> 'r'
      | 44 -> 's'
      | 45 -> 't'
      | 46 -> 'u'
      | 47 -> 'v'
      | 48 -> 'w'
      | 49 -> 'x'
      | 50 -> 'y'
      | 51 -> 'z'
      | 52 -> '0'
      | 53 -> '1'
      | 54 -> '2'
      | 55 -> '3'
      | 56 -> '4'
      | 57 -> '5'
      | 58 -> '6'
      | 59 -> '7'
      | 60 -> '8'
      | 61 -> '9'
      | 62 -> '+'
      | 63 -> '/'
      | _ -> failwith "Only valid for integers in the range 0-63"

    let to_int t =
      match t with
      | 'A' -> 0
      | 'B' -> 1
      | 'C' -> 2
      | 'D' -> 3
      | 'E' -> 4
      | 'F' -> 5
      | 'G' -> 6
      | 'H' -> 7
      | 'I' -> 8
      | 'J' -> 9
      | 'K' -> 10
      | 'L' -> 11
      | 'M' -> 12
      | 'N' -> 13
      | 'O' -> 14
      | 'P' -> 15
      | 'Q' -> 16
      | 'R' -> 17
      | 'S' -> 18
      | 'T' -> 19
      | 'U' -> 20
      | 'V' -> 21
      | 'W' -> 22
      | 'X' -> 23
      | 'Y' -> 24
      | 'Z' -> 25
      | 'a' -> 26
      | 'b' -> 27
      | 'c' -> 28
      | 'd' -> 29
      | 'e' -> 30
      | 'f' -> 31
      | 'g' -> 32
      | 'h' -> 33
      | 'i' -> 34
      | 'j' -> 35
      | 'k' -> 36
      | 'l' -> 37
      | 'm' -> 38
      | 'n' -> 39
      | 'o' -> 40
      | 'p' -> 41
      | 'q' -> 42
      | 'r' -> 43
      | 's' -> 44
      | 't' -> 45
      | 'u' -> 46
      | 'v' -> 47
      | 'w' -> 48
      | 'x' -> 49
      | 'y' -> 50
      | 'z' -> 51
      | '0' -> 52
      | '1' -> 53
      | '2' -> 54
      | '3' -> 55
      | '4' -> 56
      | '5' -> 57
      | '6' -> 58
      | '7' -> 59
      | '8' -> 60
      | '9' -> 61
      | '+' -> 62
      | '/' -> 63
      | _ -> failwith "Only valid for characters in [A-Za-z0-9+/]"
  end

  let four_to_three a b c d =
    let first = (a lsl 2) lor (b lsr 4) in
    let second = ((b land 0xf) lsl 4) lor (c lsr 2) in
    let third = ((c land 0x3) lsl 6) lor d in
    [first; second; third] |> CCList.map CCChar.of_int_exn |> CCString.of_list

  let three_to_two a b c =
    let first = (a lsl 2) lor (b lsr 4) in
    let second = ((b land 0xf) lsl 4) lor (c lsr 2) in
    [first; second] |> CCList.map CCChar.of_int_exn |> CCString.of_list

  let two_to_one a b =
    let first = (a lsl 2) lor (b lsr 4) in
    [first] |> CCList.map CCChar.of_int_exn |> CCString.of_list

  let of_base64 s =
    let s = Util.remove_whitespace s in
    assert (CCString.length s mod 4 = 0);
    let ints =
      s
      |> CCString.rdrop_while (fun c -> c = '=')
      |> CCString.to_list
      |> CCList.map Char.to_int
    in
    let rec worker ns acc =
      match ns with
      | a :: b :: c :: d :: tail -> worker tail (acc ^ four_to_three a b c d)
      | [a; b; c] -> acc ^ three_to_two a b c
      | [a; b] -> acc ^ two_to_one a b
      | [a] -> failwith ("Malformed Base 64 string: " ^ string_of_int a)
      | [] -> acc
    in
    worker ints "" |> Bytes.of_string

  let three_to_four a b c =
    let first = a lsr 2 in
    let second = ((a land 0x3) lsl 4) lor (b lsr 4) in
    let third = ((b land 0xf) lsl 2) lor (c lsr 6) in
    let fourth = c land 0x3f in
    [first; second; third; fourth] |> CCList.map Char.of_int |> CCString.of_list

  let two_to_three a b =
    let first = a lsr 2 in
    let second = ((a land 0x3) lsl 4) lor (b lsr 4) in
    let third = (b land 0xf) lsl 2 in
    [first; second; third] |> CCList.map Char.of_int |> CCString.of_list

  let one_to_two a =
    let first = a lsr 2 in
    let second = (a land 0x3) lsl 4 in
    [first; second] |> CCList.map Char.of_int |> CCString.of_list

  let to_base64 s =
    let ints = s |> Bytes.to_char_list |> CCList.map CCChar.to_int in
    let rec worker chars acc =
      match chars with
      | a :: b :: c :: tail -> worker tail (acc ^ three_to_four a b c)
      | [a; b] -> acc ^ two_to_three a b ^ "="
      | [a] -> acc ^ one_to_two a ^ "=="
      | [] -> acc
    in
    worker ints ""
end

let hex_to_base64 s = s |> Hex.of_hex_string |> Base64.to_base64

module CharMap = CCMap.Make (struct
  type t = char

  let compare = compare
end)

let etaoin =
  [ (' ', 2000)
  ; ('E', 1249)
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
    assert (m > 0);
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
    assert (Bytes.length s = Bytes.length s');
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
    assert (Bytes.length key = 16);
    assert (Bytes.length msg mod 16 = 0);
    let key =
      key |> Bytes.to_string |> Cstruct.of_string |> AES.ECB.of_secret
    in
    let msg = msg |> Bytes.to_string |> Cstruct.of_string in
    AES.ECB.encrypt ~key msg |> Cstruct.to_string |> Bytes.of_string

  let decrypt ~key msg =
    assert (Bytes.length key = 16);
    assert (Bytes.length msg mod 16 = 0);
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
