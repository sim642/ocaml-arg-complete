open OUnit2

let speclist: (Arg.key * Arg_complete.spec * Arg.doc) list = [
  ("--unit", Unit (fun () -> print_endline "unit"), "Unit");
  ("--bool", Bool (fun b -> Printf.printf "bool: %B" b), "Bool");
  ("--set", Set (ref false), "Set");
  ("--clear", Clear (ref false), "Clear");
  ("--string", String ((fun s -> Printf.printf "string: %s" s), (Arg_complete.complete_strings ["a"; "b"; "c"])), "String");
  ("--set_string", Set_string (ref "", (Arg_complete.complete_strings ["a"; "b"; "c"])), "Set_string");
  ("--symbol", Symbol (["a"; "b"; "c"], (fun s -> Printf.printf "symbol: %s" s)), "Symbol");
  ("--tuple", Tuple [Bool (fun b -> Printf.printf "bool: %B" b); Symbol (["a"; "b"; "c"], (fun s -> Printf.printf "symbol: %s" s))], "Tuple")
]

let anon_fun: Arg.anon_fun = fun s ->
  Printf.printf "anon: %s" s

let anon_complete: Arg_complete.complete = fun _s ->
  failwith "anon_complete"

let usage_msg: Arg.usage_msg = "usage_msg"

(* let () =
  Arg.parse_argv [|"program"; "--unit"|] speclist anon_fun usage_msg *)

let all_keys = List.map (fun (key, _, _) -> key) speclist

let test_key _ =
  assert_equal ["--unit"] (Arg_complete.complete_argv [|"program"; "--uni"|] speclist anon_complete);
  assert_equal ["--string"] (Arg_complete.complete_argv [|"program"; "--str"|] speclist anon_complete);
  assert_equal all_keys (Arg_complete.complete_argv [|"program"; "--"|] speclist anon_complete);
  assert_equal all_keys (Arg_complete.complete_argv [|"program"; ""|] speclist anon_complete);
  assert_equal [] (Arg_complete.complete_argv [|"program"; "--unit"|] speclist anon_complete)

let test_unit _ =
  assert_equal ["--unit"] (Arg_complete.complete_argv [|"program"; "--unit"; "--un"|] speclist anon_complete)

let test_bool _ =
  assert_equal ["false"; "true"] (Arg_complete.complete_argv [|"program"; "--bool"; ""|] speclist anon_complete);
  assert_equal ["true"] (Arg_complete.complete_argv [|"program"; "--bool"; "t"|] speclist anon_complete);
  assert_equal all_keys (Arg_complete.complete_argv [|"program"; "--bool"; "true"; ""|] speclist anon_complete)

let test_set _ =
  assert_equal all_keys (Arg_complete.complete_argv [|"program"; "--set"; ""|] speclist anon_complete)

let test_clear _ =
  assert_equal all_keys (Arg_complete.complete_argv [|"program"; "--clear"; ""|] speclist anon_complete)

let test_string _ =
  assert_equal ["a"; "b"; "c"] (Arg_complete.complete_argv [|"program"; "--string"; ""|] speclist anon_complete);
  assert_equal ["a"] (Arg_complete.complete_argv [|"program"; "--string"; "a"|] speclist anon_complete);
  assert_equal all_keys (Arg_complete.complete_argv [|"program"; "--string"; "a"; ""|] speclist anon_complete)

let test_set_string _ =
  assert_equal ["a"; "b"; "c"] (Arg_complete.complete_argv [|"program"; "--set_string"; ""|] speclist anon_complete);
  assert_equal ["a"] (Arg_complete.complete_argv [|"program"; "--set_string"; "a"|] speclist anon_complete);
  assert_equal all_keys (Arg_complete.complete_argv [|"program"; "--set_string"; "a"; ""|] speclist anon_complete)

let test_symbol _ =
  assert_equal ["a"; "b"; "c"] (Arg_complete.complete_argv [|"program"; "--symbol"; ""|] speclist anon_complete);
  assert_equal ["a"] (Arg_complete.complete_argv [|"program"; "--symbol"; "a"|] speclist anon_complete);
  assert_equal all_keys (Arg_complete.complete_argv [|"program"; "--symbol"; "a"; ""|] speclist anon_complete)

let test_tuple _ =
  assert_equal ["false"; "true"] (Arg_complete.complete_argv [|"program"; "--tuple"; ""|] speclist anon_complete);
  assert_equal ["true"] (Arg_complete.complete_argv [|"program"; "--tuple"; "t"|] speclist anon_complete);
  assert_equal ["a"; "b"; "c"] (Arg_complete.complete_argv [|"program"; "--tuple"; "true"; ""|] speclist anon_complete);
  assert_equal ["a"] (Arg_complete.complete_argv [|"program"; "--tuple"; "true"; "a"|] speclist anon_complete);
  assert_equal all_keys (Arg_complete.complete_argv [|"program"; "--tuple"; "true"; "a"; ""|] speclist anon_complete)

let tests =
  "arg_complete_test" >::: [
    "key" >:: test_key;
    "unit" >:: test_unit;
    "bool" >:: test_bool;
    "set" >:: test_set;
    "clear" >:: test_clear;
    "string" >:: test_string;
    "set_string" >:: test_set_string;
    "symbol" >:: test_symbol;
    "tuple" >:: test_tuple;
  ]

let () = run_test_tt_main tests
