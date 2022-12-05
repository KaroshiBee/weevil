module Js = Data_encoding.Json

module Ev_runscript = struct
  type t = {cmd: string}
  let enc =
    let open Data_encoding in
    (conv
      (fun {cmd} -> cmd)
      (fun cmd -> {cmd})
      (obj1 (req "cmd" string))
    )
end

module Ev_terminate = struct
  type t = unit
  let enc =
    let open Data_encoding in
    (conv
      (fun () -> ())
      (fun () -> ())
      (constant "Terminate")
    )
end

module Ev_step = struct
  type t = {step_size: int}
  let enc =
    let open Data_encoding in
    (conv
      (fun {step_size} -> step_size)
      (fun step_size -> {step_size})
      (obj1 (req "step_size" int31))
    )
end

module Ev_getrecords = struct
  type t = unit
  let enc =
    let open Data_encoding in
    (conv
      (fun () -> ())
      (fun () -> ())
      (constant "GetRecords")
    )
end


type ev =
  | RunScript of Ev_runscript.t
  | Terminate of Ev_terminate.t
  | Step of Ev_step.t
  | GetRecords of Ev_getrecords.t

type t = {
  event: ev;
}

let make ~event () = {event;}

let enc_ev =
  let open Data_encoding in
  union [

    case ~title:"RunScript" (Tag 0)
      Ev_runscript.enc
      (function RunScript t -> Some t | _ -> None)
      (fun t -> RunScript t);

    case ~title:"Terminate" (Tag 1)
      Ev_terminate.enc
      (function Terminate t -> Some t | _ -> None)
      (fun t -> Terminate t);

    case ~title:"Step" (Tag 2)
      Ev_step.enc
      (function Step t -> Some t | _ -> None)
      (fun t -> Step t);

    case ~title:"GetRecords" (Tag 3)
      Ev_getrecords.enc
      (function GetRecords t -> Some t | _ -> None)
      (fun t -> GetRecords t);
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
