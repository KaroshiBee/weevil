type t = Data_encoding.Json.json

exception Wrong_encoder of string

val from_string : string -> (t, string) Result.t

val to_string : t -> string

val construct : 'a Data_encoding.t -> 'a -> t

val destruct : 'a Data_encoding.t -> t -> 'a
