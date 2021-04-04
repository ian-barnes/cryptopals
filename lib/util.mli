module Int : sig
  type t = int

  val range : t -> t -> t list
end

module String : sig
  type t = string

  val remove_whitespace : t -> t
end

module List : sig
  type 'a t = 'a list

  val to_pairs : 'a t -> ('a * 'a) t

  val stripes : int -> 'a t -> 'a t t
end

module Char : sig
  type t = char

  val xor : t -> t -> t
end
