let assert_with msg condition =
  try assert condition with
  | Assert_failure _ -> failwith msg
