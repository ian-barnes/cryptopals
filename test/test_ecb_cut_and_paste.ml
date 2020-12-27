open Lib.Ecb_cut_and_paste

let role = Alcotest.testable User_profile.Role.pp User_profile.Role.equal

let test ~cookie ~expected () =
  let actual = cookie |> Server.validate |> User_profile.role in
  Alcotest.(check role) "Role" expected actual

let tests =
  [ Alcotest.test_case "Server" `Quick
      (test
         ~cookie:(Server.generate_cookie_for "foo@bar.com")
         ~expected:User_profile.Role.User)
  ; Alcotest.test_case "Client" `Quick
      (test
         ~cookie:(Client.get_admin_cookie ())
         ~expected:User_profile.Role.Admin) ]
