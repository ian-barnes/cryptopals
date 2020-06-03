val random_aes_key : unit -> Bytes.t

module Block_mode : sig
  type t =
    | ECB
    | CBC

  val to_string : t -> string
end

val encrypt : Block_mode.t -> Bytes.t -> Bytes.t

val random_encrypt : Bytes.t -> Bytes.t

val oracle : (Bytes.t -> Bytes.t) -> Block_mode.t
