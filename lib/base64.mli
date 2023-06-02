module Char : sig
  type t = char

  val of_int : int -> t
  val to_int : t -> int
end

type t = string

val decode : t -> Bytes.t
val encode : Bytes.t -> t
