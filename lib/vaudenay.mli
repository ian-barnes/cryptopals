module Server : sig
  val encrypt : ?iv:Bytes.t -> string -> Bytes.t * Bytes.t

  val oracle : ?iv:Bytes.t -> Bytes.t -> bool
end

module Client : sig
  val crack : ciphertext:Bytes.t -> iv:Bytes.t -> string
end
