module CharMap = CCMap.Make (struct
  type t = char

  let compare = compare
end)

let etaoin =
  [ (' ', 2000) (* rough guess: almost twice as many spaces as E's *)
  ; ('E', 1249) (* remainder from Norvig *)
  ; ('T', 928)
  ; ('A', 804)
  ; ('O', 764)
  ; ('I', 757)
  ; ('N', 723)
  ; ('S', 651)
  ; ('R', 628)
  ; ('H', 505)
  ; ('L', 407)
  ; ('D', 382)
  ; ('C', 334)
  ; ('U', 273)
  ; ('M', 251)
  ; ('F', 240)
  ; ('P', 214)
  ; ('G', 187)
  ; ('W', 168)
  ; ('Y', 166)
  ; ('B', 148)
  ; ('V', 105)
  ; ('K', 54)
  ; ('X', 23)
  ; ('J', 16)
  ; ('Q', 12)
  ; ('Z', 9) ]
  |> CCList.map (fun (c, n) -> (CCChar.lowercase_ascii c, n))
  |> CharMap.of_list

let frequencies (s : Bytes.t) =
  let f = function
    | Some v -> Some (v + 1)
    | None -> Some 1
  in
  Bytes.fold (fun m c -> CharMap.update c f m) CharMap.empty s

let normalise (m : int CharMap.t) : float CharMap.t =
  let total = CharMap.fold (fun _ n acc -> n + acc) m 0 in
  CharMap.map (fun n -> float_of_int n /. float_of_int total) m

let not_chi_squared (a : float CharMap.t) (b : float CharMap.t) : float =
  (* Assume both a and b are already normalised *)
  let squared_differences =
    let f _c = function
      | `Both (x, y) -> Some ((x -. y) *. (x -. y))
      | `Left x -> Some (x *. x)
      | `Right y -> Some (y *. y)
    in
    CharMap.merge_safe ~f a b
  in
  CharMap.fold (fun _ x acc -> x +. acc) squared_differences 0.0

let score (b : Bytes.t) : float =
  not_chi_squared
    (normalise (frequencies (Bytes.lowercase_ascii b)))
    (normalise etaoin)

let crack (s : Bytes.t) : char * Bytes.t =
  let mask c =
    CCString.repeat (CCChar.to_string c) (Bytes.length s) |> Bytes.of_string
  in
  CharMap.empty
  |> CCList.fold_right
       (fun c m -> CharMap.add c (Bytes.xor s (mask c)) m)
       (CCList.map CCChar.of_int_exn (Util.range 0 255))
  |> CharMap.map (fun s -> (s, score s))
  |> CharMap.to_list
  |> CCList.sort (fun (_, (_, x)) (_, (_, x')) -> compare x x')
  |> CCList.hd
  |> fun (key, (plaintext, _)) -> (key, plaintext)

let decode b = b |> crack |> fun (_key, plaintext) -> plaintext

let detect b =
  b
  |> CCList.map (fun line ->
         let plaintext = decode line in
         (plaintext, score plaintext))
  |> CCList.sort (fun (_, score) (_, score') -> compare score score')
  |> CCList.hd
  |> fst
