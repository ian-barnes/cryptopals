module User_profile : sig
  module Role : sig
    type t =
      | User
      | Admin
    [@@deriving eq, show]

    val to_string : t -> string
    val of_string : string -> t
  end

  type t =
    { email : string
    ; uid : int
    ; role : Role.t }

  val email : t -> string
  val uid : t -> int
  val role : t -> Role.t
  val make : email:string -> ?uid:int -> ?role:Role.t -> unit -> t
  val encode : t -> string
  val parse : string -> t
  val to_json : t -> Yojson.Safe.t
end

module Server : sig
  val generate_cookie_for : string -> Bytes.t
  val validate : Bytes.t -> User_profile.t
end

module Client : sig
  val get_admin_cookie : unit -> Bytes.t
end
