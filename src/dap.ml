module Encodings = struct

  open Json_encoding

  type message_type = | Request | Response | Event
  let protocol_message =
    obj2
      (req "seq" int)
      (req "type" @@ string_enum [("request", Request);
                                  ("response", Response);
                                  ("event", Event);
                                 ])




  let request =
    merge_objs
      protocol_message
      (obj3
         (req "type" @@ string_enum [("request", Request)])
         (req "command" string)
         (opt "arguments" @@ list any_value)
      )

end


let example_msg =
  let seq = 1234 in
  let type_ = Encodings.Request in
  Json_encoding.construct Encodings.protocol_message (seq, type_)

let example_request =
  let seq = 1234 in
  let type_ = Encodings.Request in
  Json_encoding.construct Encodings.request ((seq, type_), (Encodings.Request, "example command", None))


let _ = (example_msg, example_request)
