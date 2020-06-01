val encode : key:Bytes.t -> Bytes.t -> Bytes.t

val decode : key:Bytes.t -> Bytes.t -> Bytes.t

val hamming_weight : Bytes.t -> int

val hamming_distance : Bytes.t -> Bytes.t -> int

val key_length_score : Bytes.t -> int -> float

val best_guess_key_length : Bytes.t -> int

val guess_key : Bytes.t -> Bytes.t

val crack : Bytes.t -> Bytes.t * Bytes.t
