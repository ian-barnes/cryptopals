let encode ~(key : Bytes.t) (msg : Bytes.t) =
  let m = Bytes.length key in
  Assert.assert_with "key length must be positive" (m > 0);
  let n = Bytes.length msg in
  let key = Bytes.repeat key (1 + (n / m)) |> Bytes.take n in
  Bytes.xor key msg

let decode = encode

let hamming_weight (s : Bytes.t) =
  let weight n =
    let rec worker n count =
      match n with
      | 0 -> count
      | n -> worker (n land (n - 1)) (count + 1)
    in
    worker n 0
  in
  s
  |> Bytes.to_char_list
  |> CCList.map CCChar.to_int
  |> CCList.map weight
  |> CCList.fold_left (fun a b -> a + b) 0

let hamming_distance (s : Bytes.t) (s' : Bytes.t) =
  Assert.assert_with "lengths must be equal" (Bytes.length s = Bytes.length s');
  Bytes.xor s s' |> hamming_weight

let key_length_score (s : Bytes.t) len =
  let rec worker start acc =
    if start + len > Bytes.length s then
      acc
    else
      worker (start + len) (Bytes.sub s start len :: acc)
  in
  let blocks = worker 0 [] in
  let pairs = CCList.diagonal blocks in
  let scores =
    CCList.map
      (fun (a, b) -> Float.of_int (hamming_distance a b) /. Float.of_int len)
      pairs
  in
  if CCList.length scores = 0 then
    8.0
  else
    CCList.fold_left (fun x y -> x +. y) 0.0 scores
    /. Float.of_int (CCList.length scores)

let best_guess_key_length (s : Bytes.t) =
  let scores =
    Util.range 2 40
    |> CCList.map (fun length -> (length, key_length_score s length))
    |> CCList.sort (fun (_, score) (_, score') -> compare score score')
  in
  (* CCList.iter
     (fun (len, score) -> print_endline (Printf.sprintf "%d -> %f" len score))
     (scores |> CCList.take 10); *)
  scores |> CCList.hd |> fst

let guess_key (b : Bytes.t) =
  (* Guess key length *)
  let n = best_guess_key_length b in
  (* Split ciphertext into bands by index mod key length *)
  let stripes = Util.stripes n b in
  (* Find the single character key for each band *)
  let keys =
    CCList.map (fun bs -> bs |> Single_char_xor.crack |> fst) stripes
  in
  (* Assemble the characters to get the actual key *)
  keys |> Bytes.of_char_list

let crack s =
  let key = guess_key s in
  (key, decode ~key s)
