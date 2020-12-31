module Server : sig
  val oracle : Bytes.t -> Bytes.t
end

module Client : sig
  val zeros : int -> Bytes.t

  val chars : char list

  module BlockMap : CCMap.S with type key = Bytes.t

  type dictionary = char BlockMap.t

  val blocksize_and_length : (Bytes.t -> Bytes.t) -> int * int

  val decrypt : (Bytes.t -> Bytes.t) -> Bytes.t
end
