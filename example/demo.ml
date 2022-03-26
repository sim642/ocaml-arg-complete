let anon_fun: Arg.anon_fun = fun s ->
  Printf.printf "anon: %s" s

let anon_complete: Arg_complete.complete = Arg_complete.complete_strings ["bar"; "baz"]

let complete_file _s =
  [] (* -o default in script changes empty completion to default filename completion *)

let rec speclist: (Arg.key * Arg_complete.spec * Arg.doc) list = [
  ("--unit", Unit (fun () -> print_endline "unit"), "Unit");
  ("--bool", Bool (fun b -> Printf.printf "bool: %B" b), "Bool");
  ("--set", Set (ref false), "Set");
  ("--clear", Clear (ref false), "Clear");
  ("--string", String ((fun s -> Printf.printf "string: %s" s), (Arg_complete.complete_strings ["a"; "b"; "c"])), "String");
  ("--set_string", Set_string (ref "", (Arg_complete.complete_strings ["a"; "b"; "c"])), "Set_string");
  ("--symbol", Symbol (["a"; "b"; "c"], (fun s -> Printf.printf "symbol: %s" s)), "Symbol");
  ("--tuple", Tuple [Bool (fun b -> Printf.printf "bool: %B" b); Symbol (["a"; "b"; "c"], (fun s -> Printf.printf "symbol: %s" s))], "Tuple");
  ("--", Rest ((fun s -> Printf.printf "rest: %s" s), (Arg_complete.complete_strings ["foo"; "bar"])), "Rest");
  ("-+", Rest_all ((fun _l -> Printf.printf "rest_all"), (function [arg] -> Arg_complete.complete_strings ["foo"; "bar"] arg | [_; arg] -> Arg_complete.complete_strings ["bar"] arg | _ -> failwith "too many args")), "Rest_all");
  ("--expand", Expand (fun s -> [|s; s|]), "Expand");
  ("--complete", Rest_all (complete_all, fun _ -> failwith "complete complete"), "Complete");
  ("--file", String ((fun f -> Printf.printf "file: %s" f), complete_file), "file");
]
and complete_all argv =
  let completions = Arg_complete.complete_argv argv speclist anon_complete in
  List.iter print_endline completions

let usage_msg: Arg.usage_msg = "usage_msg"

let () =
  Arg.parse (Arg_complete.arg_speclist speclist) anon_fun usage_msg
