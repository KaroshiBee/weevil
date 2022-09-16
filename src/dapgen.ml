open Dapper_gen.Dap_dfs

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Debug);
  let schema_js = Ezjsonm.from_channel @@ open_in "./schema/errorResponse.json" in
  (* let schema_js = Ezjsonm.from_channel @@ open_in "./schema/debugAdapterProtocol-1.56.X.json" in *)
  let _ = Dfs.make ~schema_js in
  ()
