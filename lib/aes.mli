val blocksize : int
val random_key : unit -> Bytes.t
val encrypt_single_block : key:Bytes.t -> Bytes.t -> Bytes.t
val decrypt_single_block : key:Bytes.t -> Bytes.t -> Bytes.t
