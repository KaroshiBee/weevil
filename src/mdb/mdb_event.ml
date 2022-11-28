module Js = Data_encoding.Json

type ev =
  | RunScript of string
  | Terminate
  | Step of int

type t = {
  event: ev;
}

let make ~event () = {event;}

let enc_ev =
  let open Data_encoding in
  union [
    case ~title:"RunScript" (Tag 0)
      string
      (function RunScript s -> Some s | _ -> None)
      (fun s -> RunScript s);
    case ~title:"Terminate" (Tag 1)
      empty
      (function Terminate -> Some () | _ -> None)
      (fun _ -> Terminate);
    case ~title:"Step" (Tag 2)
      int31
      (function Step n -> Some n | _ -> None)
      (fun n -> Step n);
  ]

let enc =
  let open Data_encoding in
  conv
    (function {event;} -> event)
    (fun event -> {event;})
    (obj1
       (req "event" enc_ev))

let from_msg_opt msg =
  try
    (* TODO be better *)
    let r : t = Js.(from_string msg |> Stdlib.Result.get_ok |> destruct enc) in
    Option.Some (r.event)
  with _ ->
    None
