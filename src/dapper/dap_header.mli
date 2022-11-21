val wrap : ?add_header:bool -> string -> string
val content_length : string -> int option
val content_length_message_handler :
  name:string ->
  handle_message: (string -> Lwt_io.input_channel -> Lwt_io.output_channel -> unit Lwt.t) ->
  content_length:int option ->
  Lwt_io.input_channel ->
  Lwt_io.output_channel ->
  unit Lwt.t
