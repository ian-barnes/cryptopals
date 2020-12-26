open Lib.Ecb_cbc_detection_oracle

let _ = Random.self_init ()

let test_once ~mode =
  let cipher = encryption_oracle_helper mode in
  block_mode_detector cipher = mode

let test ~mode () =
  let open Lib.Util in
  let expected = true in
  let actual = range 1 1000 |> CCList.for_all (fun _ -> test_once ~mode) in
  Alcotest.(check bool) ("Detect " ^ Block_mode.to_string mode) expected actual

let test_random () =
  let open Lib.Util in
  let expected = true in
  let actual =
    range 1 1000
    |> CCList.for_all (fun _ ->
           let mode =
             if Random.bool () then
               Block_mode.ECB
             else
               Block_mode.CBC
           in
           test_once ~mode)
  in
  Alcotest.(check bool) "Detect block mode" expected actual

let tests =
  [ Alcotest.test_case "ECB mode" `Slow (test ~mode:Block_mode.ECB)
  ; Alcotest.test_case "CBC mode" `Slow (test ~mode:Block_mode.CBC)
  ; Alcotest.test_case "Random mode" `Slow test_random ]
