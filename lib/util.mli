val range : int -> int -> int list

val remove_whitespace : string -> string

val printable : string -> string

val wrap : int -> string -> string

val char_to_hex : char -> string

module List : sig
  type 'a t = 'a list

  val to_pairs : 'a t -> ('a * 'a) t

  val stripes : int -> 'a t -> 'a t t
end

module Char : sig
  type t = char

  val xor : t -> t -> t
end
