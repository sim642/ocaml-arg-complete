let anon_fun: Arg.anon_fun = Printf.printf "anon: %s\n"
let anon_complete: Arg_complete.complete = Arg_complete.strings ["bar"; "baz"]

let rec speclist: Arg_complete.speclist = [
  ("--unit", Unit (fun () -> Printf.printf "unit\n"), "Unit");
  ("--bool", Bool (Printf.printf "bool: %B\n"), "Bool");
  ("--string", String (Printf.printf "string: %s\n", Arg_complete.strings ["a"; "b"; "c"]), "String");
  ("--tuple", Tuple [Bool (Printf.printf "tuple bool: %B\n"); Symbol (["a"; "b"; "c"], Printf.printf "tuple symbol: %s\n")], "Tuple");
  ("--file", String (Printf.printf "file: %s\n", Arg_complete.empty), "File"); (* -o default in script changes empty completion to default filename completion *)
  ("--complete", Rest_all (complete, Arg_complete.empty_all), "Complete");
]
and complete args =
  Arg_complete.complete_argv args speclist anon_complete
  |> List.iter print_endline


let () =
  let arg_speclist = Arg_complete.arg_speclist speclist in
  let usage_msg = "Usage:" in
  Arg.parse arg_speclist anon_fun usage_msg
