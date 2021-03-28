(* Challenge 13 *)

(* A data module for storing the user profiles that are the subject of this
   exercise *)

module User_profile = struct
  module Role = struct
    type t =
      | User
      | Admin
    [@@deriving eq, show]

    let to_string = function
      | User -> "user"
      | Admin -> "admin"

    let of_string s =
      match s with
      | "user" -> User
      | "admin" -> Admin
      | _ -> failwith "bad role"
  end

  type t =
    { email : string
    ; uid : int
    ; role : Role.t }

  let email t = t.email

  let uid t = t.uid

  let role t = t.role

  let make ~email ?(uid = 10) ?(role = Role.User) () = {email; uid; role}

  let encode t =
    "email="
    ^ t.email
    ^ "&uid="
    ^ string_of_int t.uid
    ^ "&role="
    ^ Role.to_string t.role

  let pairs_to_json (s : string) : Yojson.Safe.t =
    `Assoc
      (s
      |> CCString.split ~by:"&"
      |> CCList.map (fun s -> CCString.Split.left_exn ~by:"=" s)
      |> CCList.map (fun (x, y) -> (x, `String y)))

  let parse s =
    let open Yojson.Basic.Util in
    let json = s |> pairs_to_json |> Yojson.Safe.to_basic in
    let email = json |> member "email" |> to_string in
    let uid = json |> member "uid" |> to_string |> int_of_string in
    let role = json |> member "role" |> to_string |> Role.of_string in
    make ~email ~uid ~role ()

  let to_json t =
    `Assoc
      [ ("email", `String t.email)
      ; ("uid", `Int t.uid)
      ; ("role", `String (Role.to_string t.role)) ]
end

module Server = struct
  (* The server that is getting attacked. It can receive an email address and
     return a new user profile with that email address, a uid (always 10 in this
     dummy example) and the plain user role. This is encoded to look like URL
     GET arguments and then encrypted using AES/ECB with a key known only to the
     server and the encrypted blob is sent back to the client, something like a
     cookie. The client can also send a cookie/ciphertext for validation, and if
     it checks out, the server will return a user profile object. The goal here
     is to trick it into validating us as an admin user. *)
  let key = Aes.random_key ()

  let profile_for (email : string) : User_profile.t =
    (* Make sure this can't be hacked by special characters in the email address *)
    let email = email |> CCString.filter (fun c -> c != '=' && c != '&') in
    User_profile.make ~email ()

  let encrypt profile =
    profile
    |> User_profile.encode
    |> Bytes.of_string
    |> Pkcs7_padding.pad
    |> Aes_ecb_mode.encrypt ~key

  let decrypt bytes =
    bytes
    |> Aes_ecb_mode.decrypt ~key
    |> Pkcs7_padding.unpad
    |> Bytes.to_string
    |> User_profile.parse

  let generate_cookie_for email = email |> profile_for |> encrypt

  let validate cookie = cookie |> decrypt
end

module Client = struct
  let get_admin_cookie () =
    (* We know block length = 16, and we know the encoding format puts the
       string "email=" of length 6 before the email address. Construct an email
       address such that the second block of plaintext will be "admin" correctly
       PKCS#7 padded. Send it to the oracle and take just the second block of
       the ciphertext. *)
    let email = "1234567890" ^ "admin" ^ CCString.make 11 '\x0b' in
    let cookie = Server.generate_cookie_for email in
    let admin_block = cookie |> Blocks.of_bytes |> CCList.get_at_idx_exn 1 in

    (* Second evil email is the right length that the last block of the encoded
       profile is just the string "user" *)
    let email = "1234567890123" in
    let cookie = Server.generate_cookie_for email in

    (* Now construct our malicious cookie by replacing the last block of that
       cookie with the "admin" block from the first cookie *)
    let blocks = Blocks.of_bytes cookie in
    let block_count = CCList.length blocks in
    CCList.append (CCList.take (block_count - 1) blocks) [admin_block]
    |> Blocks.to_bytes
end
