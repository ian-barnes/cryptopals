open Lib

let () =
  let open Ecb_cbc_detection_oracle in
  let test mode =
    let cipher = encrypt mode in
    Util.range 1 1000
    |> CCList.fold_left
         (fun count _ ->
           if oracle cipher = mode then
             count
           else
             count + 1)
         0
  in

  Printf.printf "ECB mode: number of errors = %d\n" (test Block_mode.ECB);
  Printf.printf "CBC mode: number of errors = %d\n" (test Block_mode.CBC)
