open Protocol
open Alpha_context


module LocHashtbl = Hashtbl.Make(struct
    type t = Script.location
    let equal i j = i=j
    let hash i = i land max_int
  end)

type 'log_element log_record = | New of 'log_element | Old of 'log_element
let make_new l = New l
let make_old l = Old l

type 'log_element t = 'log_element log_record LocHashtbl.t
let make () = LocHashtbl.create 500

let add_new t loc log_element =
  LocHashtbl.add t loc @@ make_new log_element

let add_old t loc log_element =
  LocHashtbl.add t loc @@ make_old log_element

let mem t = LocHashtbl.mem t
let to_list t =
  LocHashtbl.to_seq t
  |> List.of_seq
  |> List.sort (fun (locX, _) (locY, _) -> Int.compare locX locY)
  |> List.filter_map
    (function
      | (_, Old _ ) -> None
      | (_, New log_element) -> Some log_element
    )

let new_to_old_inplace ~keep_old t =
  LocHashtbl.filter_map_inplace (fun _ky -> function
      | New l -> Some (make_old l)
      | Old _ as l -> if keep_old then Some l else None
    ) t
