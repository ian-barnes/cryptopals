type t

val length : t -> int

val of_string : string -> t

val to_string : t -> string

val to_char_list : t -> char list

val of_char_list : char list -> t

val xor : t -> t -> t

val lowercase_ascii : t -> t

val fold : ('a -> char -> 'a) -> 'a -> t -> 'a

val repeat : t -> int -> t

val take : int -> t -> t

val sub : t -> int -> int -> t
