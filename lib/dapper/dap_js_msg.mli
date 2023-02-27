type t = Data_encoding.Json.json

type wrong_encoder_details = [
  | `Cannot_construct
  | `Cannot_destruct
  | `Cannot_destruct_wrong_fields
]
exception Wrong_encoder of (string * wrong_encoder_details)

val from_string : string -> (t, string) Result.t

val to_string : t -> string

val construct : 'a Data_encoding.t -> 'a -> t

val destruct : 'a Data_encoding.t -> t -> 'a
