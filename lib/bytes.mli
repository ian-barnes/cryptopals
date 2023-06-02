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
val get : int -> t -> char
val set : int -> char -> t -> t
val pad : ?side:[`Left | `Right] -> ?c:char -> int -> t -> t
val empty : t
val is_empty : t -> bool
val append : suffix:t -> t -> t
val ( || ) : t -> t -> t
val prepend : prefix:t -> t -> t
val chop_suffix_exn : msg:string -> suffix:t -> t -> t
val to_printable_string : t -> string
val compare : t -> t -> int
val of_char : char -> t
val concat : t list -> t
val zeros : length:int -> t
val stripes : int -> t -> t list
val random : int -> t
