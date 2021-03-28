module Block_mode : sig
  type t =
    | ECB
    | CBC

  val to_string : t -> string
end

val encryption_oracle_helper : Block_mode.t -> Bytes.t -> Bytes.t

val encryption_oracle : Bytes.t -> Bytes.t

val block_mode_detector : (Bytes.t -> Bytes.t) -> Block_mode.t
