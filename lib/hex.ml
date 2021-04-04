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
  let s = Util.String.remove_whitespace s in
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

let to_hex_string ?(separator = "") s =
  s
  |> Bytes.to_char_list
  |> CCList.map to_hex_pair
  |> CCList.map (fun (x, y) ->
         [Digit.of_int x; Digit.of_int y] |> CCString.of_list)
  |> CCString.concat separator
