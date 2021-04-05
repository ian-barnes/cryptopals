open Lib

let rec transpose =
  let head_opt = function
    | [] -> None
    | head :: _ -> Some head
  in
  let safe_tail = function
    | [] -> []
    | _ :: tail -> tail
  in
  function
  | [] -> []
  | [] :: _ -> []
  | rows ->
    (rows |> CCList.map head_opt |> CCList.keep_some)
    :: transpose (rows |> CCList.map safe_tail)

let transpose_bytes_list (xs : Bytes.t list) : Bytes.t list =
  xs
  |> CCList.map Bytes.to_char_list
  |> transpose
  |> CCList.map Bytes.of_char_list

let list_min (xs : int list) : int = xs |> CCList.sort compare |> CCList.hd

let () =
  ["data/19.txt"; "data/20.txt"]
  |> CCList.iter (fun input_file ->
         let msgs = File.read_lines input_file in
         let key = Aes.random_key () in
         let nonce = Bytes.zeros ~length:8 in
         let ciphertexts =
           msgs
           |> CCList.map CCFun.(Base64.decode %> Ctr_mode.encrypt ~key ~nonce)
         in
         let length = ciphertexts |> CCList.map Bytes.length |> list_min in
         ciphertexts
         |> CCList.filter (fun bytes -> Bytes.length bytes >= length)
         |> CCList.map (Bytes.take length)
         |> transpose_bytes_list
         |> CCList.map Single_char_xor.decode
         |> transpose_bytes_list
         |> CCList.iter (fun bytes ->
                Printf.printf "%s\n" (bytes |> Bytes.to_string)))
