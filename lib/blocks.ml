type t = Bytes.t list

let of_bytes ?blocksize:(n = 16) bytes =
  Assert.assert_with "bad msg length" (Bytes.length bytes mod n = 0);
  let rec worker msg acc =
    match Bytes.length msg with
    | 0 -> CCList.rev acc
    | _ ->
      let (h, t) = Bytes.take_drop n msg in
      worker t (h :: acc)
  in
  worker bytes []

let to_bytes t = Bytes.concat t

let to_printable_string t =
  t
  |> CCList.map (fun block ->
         Hex.to_hex_string block ^ " " ^ Bytes.to_printable_string block)
  |> CCString.concat "\n"
