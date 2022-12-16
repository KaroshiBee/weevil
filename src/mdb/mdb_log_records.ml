type counter = int
let start_ix = 0
let ix : counter ref = ref start_ix

module CHashtbl = Hashtbl.Make(struct
    type t = counter
    let equal i j = i=j
    let hash i = i land max_int
  end)

type 'log_element log_record = | New of 'log_element | Old of 'log_element
let make_new l = New l
let make_old l = Old l

type 'log_element t = 'log_element log_record CHashtbl.t
let make () = CHashtbl.create 500

let remove_all t =
  CHashtbl.reset t;
  ix := start_ix

let add_new t log_element =
  CHashtbl.add t !ix @@ make_new log_element;
  ix := succ !ix

let add_old t log_element =
  CHashtbl.add t !ix @@ make_old log_element;
  ix := succ !ix

let mem t = CHashtbl.mem t
let to_list t =
  CHashtbl.to_seq t
  |> List.of_seq
  |> List.sort (fun (cX, _) (cY, _) -> Int.compare cX cY)
  |> List.filter_map
    (function
      | (_, Old _ ) -> None
      | (_, New log_element) -> Some log_element
    )

let new_to_old_inplace ~keep_old t =
  CHashtbl.filter_map_inplace (fun _ky -> function
      | New l -> Some (make_old l)
      | Old _ as l -> if keep_old then Some l else None
    ) t
