let anon_fun: Arg.anon_fun = fun s ->
  Printf.printf "anon: %s" s

let anon_complete: Arg_complete.complete = Arg_complete.complete_strings ["bar"; "baz"]

let complete_file _s =
  [] (* -o default in script changes empty completion to default filename completion *)

let rec speclist: (Arg.key * Arg_complete.spec * Arg.doc) list = [
  ("--unit", Unit (fun () -> print_endline "unit"), "Unit");
  ("--bool", Bool (fun b -> Printf.printf "bool: %B" b), "Bool");
  ("--string", String ((fun s -> Printf.printf "string: %s" s), (Arg_complete.complete_strings ["a"; "b"; "c"])), "String");
  ("--symbol", Symbol (["a"; "b"; "c"], (fun s -> Printf.printf "symbol: %s" s)), "Symbol");
  ("--tuple", Tuple [Bool (fun b -> Printf.printf "bool: %B" b); Symbol (["a"; "b"; "c"], (fun s -> Printf.printf "symbol: %s" s))], "Tuple");
#if OCAML_VERSION >= (4, 12, 0)
  ("--complete", Rest_all (complete_all, fun _ -> failwith "complete complete"), "Complete");
#else
  ("--complete", Rest (complete_one, fun _ -> failwith "complete complete"), "Complete");
#endif
  ("--file", String ((fun f -> Printf.printf "file: %s" f), complete_file), "file");
]
and complete_all argv =
  let completions = Arg_complete.complete_argv argv speclist anon_complete in
  List.iter print_endline completions
#if OCAML_VERSION < (4, 12, 0)
and complete_one_acc = ref None
and complete_one arg =
  complete_one_acc := Some (
    match !complete_one_acc with
    | None -> [arg]
    | Some acc -> arg :: acc
  )
#endif

let usage_msg: Arg.usage_msg = "usage_msg"

let () =
  Arg.parse (Arg_complete.arg_speclist speclist) anon_fun usage_msg;
#if OCAML_VERSION < (4, 12, 0)
  match !complete_one_acc with
  | Some acc -> complete_all (List.rev acc)
  | None -> ()
#else
  ()
#endif
