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
  Assert.assert_with "base64: bad length" (CCString.length s mod 4 = 0);
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
