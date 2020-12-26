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

val drop : int -> t -> t

val take_drop : int -> t -> t * t

val sub : t -> int -> int -> t

val get : t -> int -> char

val pad : ?side:[`Left | `Right] -> ?c:char -> int -> t -> t

val empty : t

val is_empty : t -> bool

val to_blocks : ?blocksize:int -> t -> t list

val of_blocks : t list -> t

val append : suffix:t -> t -> t

val prepend : prefix:t -> t -> t

val chop_suffix_exn : msg:string -> suffix:t -> t -> t

val to_printable_string : t -> string

val compare : t -> t -> int

val of_char : char -> t
