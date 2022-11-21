open Dap_header
open Alcotest

module HeaderTests = struct

  let test_content_length_simple () =
    let n = content_length "Content-Length: 136" |> Option.get in
    check int "correct content length" 136 n

  let test_content_length_complex () =
    let n = content_length "tezos-weevil: Content-Length: 136" |> Option.get in
    check int "correct content length" 136 n

end
