val encrypt : key:Bytes.t -> ?nonce:Bytes.t -> Bytes.t -> Bytes.t

val decrypt : key:Bytes.t -> ?nonce:Bytes.t -> Bytes.t -> Bytes.t
