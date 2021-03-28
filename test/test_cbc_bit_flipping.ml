open Lib.Cbc_bit_flipping

let test ~input ~expected () =
  let actual = Server.got_admin input in
  Alcotest.(check bool) "Got admin" expected actual

let tests =
  [ Alcotest.test_case "Naive injection attack fails" `Quick
      (test ~input:(Client.naive_msg |> Server.encrypt) ~expected:false)
  ; Alcotest.test_case "Unmodified message fails" `Quick
      (test ~input:(Client.smart_msg |> Server.encrypt) ~expected:false)
  ; Alcotest.test_case "Bit flipping succeeds" `Quick
      (test
         ~input:(Client.smart_msg |> Server.encrypt |> Client.flip_bits)
         ~expected:true) ]
