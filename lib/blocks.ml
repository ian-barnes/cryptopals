type t = Bytes.t list

let of_bytes ?(blocksize = Aes.blocksize) bytes =
  Assert.assert_with "bad msg length" (Bytes.length bytes mod blocksize = 0);
  let rec worker msg acc =
    match Bytes.length msg with
    | 0 -> CCList.rev acc
    | _ ->
      let (h, t) = Bytes.take_drop blocksize msg in
      worker t (h :: acc)
  in
  worker bytes []

let to_bytes t = Bytes.concat t

let to_printable_string t =
  t
  |> CCList.map (fun block ->
         Hex.encode block ^ " " ^ Bytes.to_printable_string block)
  |> CCString.concat "\n"
