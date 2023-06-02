module Server : sig
  val print_blocks : Bytes.t -> unit
  val encrypt : ?iv:Bytes.t -> string -> Bytes.t
  val got_admin : ?iv:Bytes.t -> Bytes.t -> bool
end

module Client : sig
  val naive_msg : string
  val smart_msg : string
  val flip_bits : Bytes.t -> Bytes.t
  val run : unit -> unit
end
