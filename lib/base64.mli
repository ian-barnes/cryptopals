module Char : sig
  type t

  val of_int : int -> t

  val to_int : t -> int
end

val of_base64 : string -> Bytes.t

val to_base64 : Bytes.t -> string
