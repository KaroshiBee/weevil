type t = {
  script_filename:string; storage:string; parameter:string; entrypoint:string;
}

let make ~script_filename ~storage ~parameter ~entrypoint () =
  {script_filename; storage; parameter; entrypoint }

let enc =
  let open Data_encoding in
  conv
    (fun {script_filename; storage; parameter; entrypoint;} -> (script_filename, storage, parameter, entrypoint ))
    (fun (script_filename, storage, parameter, entrypoint ) -> {script_filename; storage; parameter; entrypoint })
    (obj4
       (req "script_filename" string)
       (req "storage" string)
       (req "parameter" string)
       (req "entrypoint" string)
    )
