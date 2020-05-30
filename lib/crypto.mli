module Hex : sig
  module Digit : sig
    type t

    val of_int : int -> t

    val to_int : t -> int
  end

  val of_hex_string : string -> Bytes.t

  val to_hex_string : Bytes.t -> string
end

module Base64 : sig
  module Char : sig
    type t

    val of_int : int -> t

    val to_int : t -> int
  end

  val of_base64 : string -> Bytes.t

  val to_base64 : Bytes.t -> string
end

val hex_to_base64 : string -> string

val score : Bytes.t -> int

val decode_single_char_xor : Bytes.t -> Bytes.t

val detect_single_char_xor : Bytes.t list -> Bytes.t

module Repeating_key_xor : sig
  val encode : key:Bytes.t -> Bytes.t -> Bytes.t

  val decode : key:Bytes.t -> Bytes.t -> Bytes.t

  val hamming_weight : Bytes.t -> int

  val hamming_distance : Bytes.t -> Bytes.t -> int

  val key_length_score : Bytes.t -> int -> float

  val best_guess_key_length : Bytes.t -> int

  val guess_key : Bytes.t -> Bytes.t

  val crack : Bytes.t -> Bytes.t * Bytes.t
end

module Aes_ecb_mode : sig
  val encrypt : key:Bytes.t -> Bytes.t -> Bytes.t

  val decrypt : key:Bytes.t -> Bytes.t -> Bytes.t

  val detect : Bytes.t list -> Bytes.t
end

module Pkcs7 : sig
  val pad : blocksize:int -> Bytes.t -> Bytes.t
end
