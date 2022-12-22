module T () = struct
  type t = {mutable seqr : Dap_base.Seqr.t}

  (* NOTE protocol says that seq starts at 1 *)
  let make () = {seqr = Dap_base.Seqr.make ~seq:1 ()}

  let current_seqr {seqr} = seqr

  let set_seqr t seqr = t.seqr <- seqr
end
