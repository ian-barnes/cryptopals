type t = string

let length = CCString.length

let of_string s = s

let to_string s = s

let to_char_list = CCString.to_list

let of_char_list = CCString.of_list

let xor a b =
  assert (CCString.length a = CCString.length b);
  let char_xor c c' =
    CCChar.to_int c lxor CCChar.to_int c' |> CCChar.of_int_exn
  in
  CCString.map2 char_xor a b

let lowercase_ascii = CCString.lowercase_ascii

let fold = CCString.fold

let repeat = CCString.repeat

let take = CCString.take

let sub = CCString.sub
