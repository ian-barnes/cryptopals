module Server : sig
  val oracle : Bytes.t -> Bytes.t
end

module Client : sig
  val decrypt : (Bytes.t -> Bytes.t) -> Bytes.t
end
