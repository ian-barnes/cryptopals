let range a b =
  let rec worker a b acc =
    if a > b then
      acc
    else
      worker a (b - 1) (b :: acc)
  in
  worker a b []

let remove_whitespace s =
  let remove c =
    CCString.replace ~which:`All ~sub:(CCChar.to_string c) ~by:""
  in
  s |> remove ' ' |> remove '\n' |> remove '\t' |> remove '\r'

let printable =
  CCString.map (fun c ->
      if c >= ' ' && c <= '~' then
        c
      else
        '?')

let wrap n s =
  let rec worker s acc =
    if CCString.length s > n then
      let (line, rest) = CCString.take_drop n s in
      worker rest (line :: acc)
    else
      s :: acc
  in
  worker s [] |> CCList.rev |> CCString.concat "\n"

let char_to_hex c = c |> Char.code |> Printf.sprintf "%02x"

module List = struct
  type 'a t = 'a list

  let to_pairs t =
    let rec worker acc ys =
      match ys with
      | a :: b :: cs -> worker ((a, b) :: acc) (b :: cs)
      | _ -> CCList.rev acc
    in
    worker [] t

  let stripes n t =
    let rec worker i acc = function
      | [] -> CCList.map CCList.rev acc
      | head :: tail ->
        worker
          ((i + 1) mod n)
          (CCList.mapi
             (fun j ll ->
               if i = j then
                 head :: ll
               else
                 ll)
             acc)
          tail
    in
    worker 0 (CCList.replicate n []) t
end

module Char = struct
  type t = char

  let xor t t' = CCChar.to_int t lxor CCChar.to_int t' |> CCChar.of_int_exn
end
