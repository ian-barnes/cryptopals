open Lib

let test ~expected () =
  File.write_all "foo.txt" expected;
  let actual = File.read_all "foo.txt" in
  Alcotest.(check string) "Write and read" expected actual

let tests =
  let with_final_newline = "First line\nSecond line\n" in
  let without_final_newline = "First line\nSecond line" in
  [ Alcotest.test_case "With final newline" `Quick
      (test ~expected:with_final_newline)
  ; Alcotest.test_case "Without final newline" `Quick
      (test ~expected:without_final_newline) ]
