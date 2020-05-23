module Bytes : sig
  type t

  val of_string : string -> t

  val to_string : t -> string

  val xor : t -> t -> t

  val repeating_key_xor : key:t -> t -> t
end

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

val printable : string -> string

val score : string -> int

val decode_single_char_xor : string -> string

val detect_single_char_xor : string -> string
