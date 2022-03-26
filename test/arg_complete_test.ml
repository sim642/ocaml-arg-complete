open OUnit2

let speclist: (Arg.key * Arg_complete.spec * Arg.doc) list = [
  ("--unit", Unit (fun () -> print_endline "unit"), "Unit");
  ("--string", String ((fun s -> Printf.printf "string: %s" s), (fun _s -> ["a"; "b"; "c"])), "String");
]

let anon_fun: Arg.anon_fun = fun s ->
  Printf.printf "anon: %s" s

let anon_complete: Arg_complete.complete = fun _s ->
  failwith "anon_complete"

let usage_msg: Arg.usage_msg = "usage_msg"

(* let () =
  Arg.parse_argv [|"program"; "--unit"|] speclist anon_fun usage_msg *)

let test_key _ =
  assert_equal ["--unit"] (Arg_complete.complete_argv [|"program"; "--uni"|] speclist anon_complete);
  assert_equal ["--string"] (Arg_complete.complete_argv [|"program"; "--str"|] speclist anon_complete);
  assert_equal ["--unit"; "--string"] (Arg_complete.complete_argv [|"program"; "--"|] speclist anon_complete);
  assert_equal ["--unit"; "--string"] (Arg_complete.complete_argv [|"program"; ""|] speclist anon_complete);
  assert_equal [] (Arg_complete.complete_argv [|"program"; "--unit"|] speclist anon_complete)

let test_unit _ =
  assert_equal ["--unit"] (Arg_complete.complete_argv [|"program"; "--unit"; "--un"|] speclist anon_complete)

let test_string _ =
  assert_equal ["a"; "b"; "c"] (Arg_complete.complete_argv [|"program"; "--string"; ""|] speclist anon_complete)

let tests =
  "arg_complete_test" >::: [
    "key" >:: test_key;
    "unit" >:: test_unit;
    "string" >:: test_string;
  ]

let () = run_test_tt_main tests
