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

val pad : ?side:[`Left | `Right] -> ?c:char -> int -> t -> t

val take_drop : int -> t -> t * t

val empty : t

val to_blocks : ?blocksize:int -> t -> t list

val of_blocks : t list -> t

val append : suffix:t -> t -> t

val prepend : prefix:t -> t -> t
