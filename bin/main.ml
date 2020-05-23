open Lib

let () =
  let key = "0123456789abcdefghi" in
  let ciphertext =
    "Pack my box with five dozen liquor jugs.\n\
     How vexingly quick daft zebras jump!\n\
     A quick brown fox jumps over the lazy dog.\n\
     Now is the winter of our discontent\n\
     Made glorious summer by this son of York.\n\
     What the fuck? Why isn't this thing working? Do I have a bug?\n\
     Or is it just a flaky way of doing things?\n\
     The bits per byte should be around 2.4-2.5 for the right key length,\n\
     and more like 2.8-3.0 for the wrong ones. That's not happening.\n\
     So I must have a bug. Pffff....\n\
     But no, when I do the same thing with the sample file provided\n\
     by the Cryptopals people, I get one key length with a score of 2.78,\n\
     and all the others with scores above 3.2, a pretty good separation.\n\
     Maybe it's just that this text isn't long enough to give good\n\
     results? Huh. For the sample text given in the exercise, I get\n\
     one length (29) with 2.7 and everything else around 3.3. But\n\
     that key length is long enough that we're not considering multipes\n\
     of it, and it also happens to be prime, so there are no divisors.\n\
     When I tried with length 8, the other 'hits' were either multiples\n\
     of 8: 40, 24, 16, 32, or of 4: 4, 12, 36 20. It kind of makes sense\n\
     that these might compicate things. Key length prime and greater than\n\
     half the maximum we're prepared to consider is kind of best case."
    |> Bytes.of_string
    |> Crypto.Repeating_key_xor.encode ~key:(Bytes.of_string key)
    |> Lib.Crypto.Base64.to_base64
  in
  print_endline ("ciphertext = \n" ^ ciphertext);
  print_endline
    ( "sanity check: recovered plaintext = \""
    ^ ( ciphertext
      |> Crypto.Base64.of_base64
      |> Crypto.Repeating_key_xor.decode ~key:(Bytes.of_string key)
      |> Bytes.to_string )
    ^ "\"" );
  let likely_key_length =
    ciphertext
    |> Crypto.Base64.of_base64
    |> Crypto.Repeating_key_xor.best_guess_key_length
    |> string_of_int
  in
  print_endline ("best guess key length = " ^ likely_key_length);

  (* let (key, plaintext) = ciphertext |> Base64.of_base64 |> Repeating_key_xor.crack in
     print_endline ("key = " ^ (key |> Bytes.to_string |> Lib.Util.printable));
     print_endline ("cracked plaintext = \"" ^ (plaintext |> Bytes.to_string |> Lib.Util.printable) ^ "\""); *)
  print_endline "";
  let ciphertext = Lib.File.read_all "data/6.txt" in
  print_endline ("ciphertext = \n" ^ ciphertext);
  let likely_key_length =
    ciphertext
    |> Lib.Crypto.Base64.of_base64
    |> Crypto.Repeating_key_xor.best_guess_key_length
    |> string_of_int
  in
  print_endline ("best guess key length = " ^ likely_key_length);
  let (key, plaintext) =
    ciphertext |> Crypto.Base64.of_base64 |> Crypto.Repeating_key_xor.crack
  in
  print_endline ("key (hex) = " ^ (key |> Crypto.Hex.to_hex_string));
  print_endline ("key = " ^ (key |> Bytes.to_string));
  print_endline ("cracked plaintext = \n" ^ (plaintext |> Bytes.to_string))
