type t = Bytes.t list

val of_bytes : ?blocksize:int -> Bytes.t -> t

val to_bytes : t -> Bytes.t

val to_printable_string : t -> string
