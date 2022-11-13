module T () = struct
  type t = {mutable seqr : Dap_base.Seqr.t}

  let make () = {seqr = Dap_base.Seqr.make ~seq:0 ()}

  let current_seqr {seqr} = seqr

  let set_seqr t seqr = t.seqr <- seqr
end
