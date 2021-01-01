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

let drop = CCString.drop

let take_drop = CCString.take_drop

let sub = CCString.sub

let get = CCString.get

let set n c t = CCString.set t n c

let pad = CCString.pad

let empty = ""

let is_empty = CCString.is_empty

let append ~suffix s = s ^ suffix

let ( || ) x y = append x ~suffix:y

let prepend ~prefix s = prefix ^ s

let chop_suffix_exn ~msg ~suffix s =
  Assert.assert_with msg (CCString.suffix ~suf:suffix s);
  CCString.chop_suffix ~suf:suffix s |> CCOpt.get_exn

let to_printable_string = String.escaped

let compare = compare

let of_char c = of_char_list [c]

let concat = CCString.concat ""

let zeros ?blocksize:(n = 16) () = repeat (of_char '\x00') n
