let anon_fun: Arg.anon_fun = fun s ->
  Printf.printf "anon: %s" s

let anon_complete: Arg_complete.complete = Arg_complete.complete_strings ["bar"; "baz"]

let complete_file _s =
  [] (* -o default in script changes empty completion to default filename completion *)

let rec speclist: (Arg.key * Arg_complete.spec * Arg.doc) list Lazy.t = lazy [
  ("--unit", Unit (fun () -> print_endline "unit"), "Unit");
  ("--bool", Bool (fun b -> Printf.printf "bool: %B" b), "Bool");
  ("--string", String ((fun s -> Printf.printf "string: %s" s), (Arg_complete.complete_strings ["a"; "b"; "c"])), "String");
  ("--symbol", Symbol (["a"; "b"; "c"], (fun s -> Printf.printf "symbol: %s" s)), "Symbol");
  ("--tuple", Tuple [Bool (fun b -> Printf.printf "bool: %B" b); Symbol (["a"; "b"; "c"], (fun s -> Printf.printf "symbol: %s" s))], "Tuple");
  ("--complete", Arg_complete.Rest_all_compat.spec (Lazy.force rest_all_complete), "Complete");
  ("--file", String ((fun f -> Printf.printf "file: %s" f), complete_file), "file");
]
and rest_all_complete = lazy (Arg_complete.Rest_all_compat.create complete_all (fun _ -> failwith "complete complete"))
and complete_all argv =
  let completions = Arg_complete.complete_argv argv (Lazy.force speclist) anon_complete in
  List.iter print_endline completions

let usage_msg: Arg.usage_msg = "usage_msg"

let () =
  Arg.parse (Arg_complete.arg_speclist (Lazy.force speclist)) anon_fun usage_msg;
  Arg_complete.Rest_all_compat.finish (Lazy.force rest_all_complete)
