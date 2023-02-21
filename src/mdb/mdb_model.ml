module PP = Tezos_micheline.Micheline_parser

type t = {
  location: PP.location;
  gas: string;
  stack: string list;
}

let location t = t.location
let gas t = t.gas
let stack t = t.stack

let make ~location ~gas ~stack () =
  {location; gas; stack}

let enc =
  let open Data_encoding in
  conv
    (fun {location; gas; stack} -> (location, gas, stack))
    (fun (location, gas, stack) -> {location; gas; stack})
    (obj3
       (req "location" PP.location_encoding)
       (req "gas" string)
       (req "stack" @@ list string)
    )
