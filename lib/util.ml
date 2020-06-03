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

let list_stripes n l =
  let rec worker i acc = function
    | [] -> CCList.map CCList.rev acc
    | h :: t ->
      worker
        ((i + 1) mod n)
        (CCList.mapi
           (fun j ll ->
             if i = j then
               h :: ll
             else
               ll)
           acc)
        t
  in
  worker 0 (CCList.replicate n []) l

let stripes n s =
  s |> Bytes.to_char_list |> list_stripes n |> CCList.map Bytes.of_char_list

let wrap n s =
  let rec worker s acc =
    if CCString.length s > n then
      let (line, rest) = CCString.take_drop n s in
      worker rest (line :: acc)
    else
      s :: acc
  in
  worker s [] |> CCList.rev |> CCString.concat "\n"
