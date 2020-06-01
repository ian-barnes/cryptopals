let () =
  Alcotest.run "Cryptopals challenges"
    [ ("Hex encoding", Test_hex.tests)
    ; ("Base64 encoding", Test_base64.tests)
    ; ("Challenge 1", Test_hex_to_base64.tests)
    ; ("Challenge 2", Test_xor.tests)
    ; ("Challenge 3", Test_frequency_analysis.tests)
    ; ("Challenge 4", Test_detect_single_char_xor.tests)
    ; ("Challenge 5", Test_repeating_key_xor.tests)
    ; ("Hamming distance", Test_hamming_distance.tests)
    ; ("Challenge 6", Test_break_repeating_key_xor.tests)
    ; ("Challenge 7", Test_aes_ecb_mode.tests)
    ; ("File IO", Test_file_io.tests)
    ; ("Challenge 8", Test_detect_ecb_mode.tests)
    ; ("Challenge 9", Test_pkcs7_padding.tests)
    ; ("Challenge 10", Test_cbc_mode.tests) ]
