module Digit : sig
  type t

  val of_int : int -> t

  val to_int : t -> int
end

val of_hex_string : string -> Bytes.t

val to_hex_string : Bytes.t -> string
