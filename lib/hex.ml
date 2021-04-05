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

module Pair = struct
  type t =
    { left : Digit.t
    ; right : Digit.t }

  let to_byte t =
    let left = Digit.to_int t.left in
    let right = Digit.to_int t.right in
    (16 * left) + right |> CCChar.of_int_exn

  let of_byte c =
    let n = CCChar.code c in
    let left = n lsr 4 |> Digit.of_int in
    let right = n land 0xf |> Digit.of_int in
    {left; right}
end

type t = string

let decode t =
  let chars = t |> Util.String.remove_whitespace |> CCString.to_list in
  let rec worker cs acc =
    match cs with
    | left :: right :: tail -> worker tail (Pair.to_byte {left; right} :: acc)
    | [] -> Bytes.of_char_list (CCList.rev acc)
    | _ -> failwith "Hex string with odd length"
  in
  worker chars []

let encode ?(separator = "") bytes =
  bytes
  |> Bytes.to_char_list
  |> CCList.map Pair.of_byte
  |> CCList.map (fun {Pair.left; right} -> [left; right] |> CCString.of_list)
  |> CCString.concat separator
