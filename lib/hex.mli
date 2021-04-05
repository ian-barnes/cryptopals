type t = string

val decode : t -> Bytes.t

val encode : ?separator:string -> Bytes.t -> t
