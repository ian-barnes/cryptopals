type t = string

let length = CCString.length

let of_string s = s

let to_string s = s

let to_char_list = CCString.to_list

let of_char_list = CCString.of_list

let xor a b =
  Assert.assert_with "xor: lengths must be equal"
    (CCString.length a = CCString.length b);
  let char_xor c c' =
    CCChar.to_int c lxor CCChar.to_int c' |> CCChar.of_int_exn
  in
  CCString.map2 char_xor a b

let lowercase_ascii = CCString.lowercase_ascii

let fold = CCString.fold

let repeat = CCString.repeat

let take = CCString.take

let sub = CCString.sub

let pad = CCString.pad

let take_drop = CCString.take_drop

let empty = ""

let to_blocks ?blocksize:(n = 16) msg =
  Assert.assert_with "bad msg length" (length msg mod n = 0);
  let rec worker msg acc =
    match length msg with
    | 0 -> CCList.rev acc
    | _ ->
      let (h, t) = take_drop n msg in
      worker t (h :: acc)
  in
  worker msg []

let of_blocks blocks = CCString.concat "" blocks
