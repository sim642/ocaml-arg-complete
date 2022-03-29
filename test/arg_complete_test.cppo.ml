open OUnit2

let anon_fun: Arg.anon_fun = Printf.printf "anon: %s\n"
let anon_complete: Arg_complete.complete = Arg_complete.strings ["bar"; "baz"]

let speclist: Arg_complete.speclist = [
  ("--unit", Unit (fun () -> print_endline "unit\n"), "Unit");
  ("--bool", Bool (Printf.printf "bool: %B"), "Bool");
  ("--set", Set (ref false), "Set");
  ("--clear", Clear (ref false), "Clear");
  ("--string", String (Printf.printf "string: %s\n", Arg_complete.strings ["a"; "b"; "c"]), "String");
  ("--set_string", Set_string (ref "", Arg_complete.strings ["a"; "b"; "c"]), "Set_string");
  ("--symbol", Symbol (["a"; "b"; "c"], Printf.printf "symbol: %s\b"), "Symbol");
  ("--tuple", Tuple [Bool (Printf.printf "tuple bool: %B\n"); Symbol (["a"; "b"; "c"], Printf.printf "tuple symbol: %s\n")], "Tuple");
  ("--", Rest (Printf.printf "rest: %s\n", Arg_complete.strings ["foo"; "bar"]), "Rest");
#if OCAML_VERSION >= (4, 12, 0)
  ("-+", Rest_all ((fun _l -> Printf.printf "rest_all"), (function [arg] -> Arg_complete.strings ["foo"; "bar"] arg | [_; arg] -> Arg_complete.strings ["bar"] arg | _ -> failwith "too many args")), "Rest_all");
#endif
#if OCAML_VERSION >= (4, 5, 0)
  ("--expand", Expand (fun s -> [|s; s|]), "Expand");
#endif
  ("--side_effect",
    (let tmp = ref "" in
    let spec1 = Arg_complete.Set_string (tmp, (fun s -> tmp := s; [])) in
    let spec2 = Arg_complete.String (Printf.printf "side_effect: %s\n", (fun _ -> [!tmp])) in
    Tuple [spec1; spec2]),
    "side effect");
]


let all_keys = List.map (fun (key, _, _) -> key) speclist @ ["-help"; "--help"]
let all_empty = ["bar"; "baz"]


let assert_complete expected args =
  let printer = Format.asprintf "%a" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ") Format.pp_print_string) in
  assert_equal ~printer expected (Arg_complete.complete_argv2 args speclist anon_complete)


let test_anon _ =
  assert_complete ["bar"; "baz"] ["b"];
  assert_complete ["bar"; "baz"] ["--unit"; "b"]

let test_key _ =
  assert_complete ["--unit"] ["--uni"];
  assert_complete ["--string"] ["--str"];
  assert_complete all_keys ["-"];
  assert_complete all_empty [""];
  assert_complete ["--unit"] ["--unit"]

let test_unit _ =
  assert_complete ["--unit"] ["--unit"; "--un"]

let test_bool _ =
  assert_complete ["false"; "true"] ["--bool"; ""];
  assert_complete ["true"] ["--bool"; "t"];
  assert_complete all_empty ["--bool"; "true"; ""]

let test_set _ =
  assert_complete all_empty ["--set"; ""]

let test_clear _ =
  assert_complete all_empty ["--clear"; ""]

let test_string _ =
  assert_complete ["a"; "b"; "c"] ["--string"; ""];
  assert_complete ["a"] ["--string"; "a"];
  assert_complete all_empty ["--string"; "a"; ""]

let test_set_string _ =
  assert_complete ["a"; "b"; "c"] ["--set_string"; ""];
  assert_complete ["a"] ["--set_string"; "a"];
  assert_complete all_empty ["--set_string"; "a"; ""]

let test_symbol _ =
  assert_complete ["a"; "b"; "c"] ["--symbol"; ""];
  assert_complete ["a"] ["--symbol"; "a"];
  assert_complete all_empty ["--symbol"; "a"; ""]

let test_tuple _ =
  assert_complete ["false"; "true"] ["--tuple"; ""];
  assert_complete ["true"] ["--tuple"; "t"];
  assert_complete ["a"; "b"; "c"] ["--tuple"; "true"; ""];
  assert_complete ["a"] ["--tuple"; "true"; "a"];
  assert_complete all_empty ["--tuple"; "true"; "a"; ""]

let test_rest _ =
  assert_complete (List.filter (Arg_complete.Util.starts_with ~prefix:"--") all_keys) ["--"];
  assert_complete ["foo"; "bar"] ["--"; ""];
  assert_complete ["foo"; "bar"] ["--"; "foo"; ""]

let test_rest_all _ =
#if OCAML_VERSION >= (4, 12, 0)
  assert_complete ["foo"; "bar"] ["-+"; ""];
  assert_complete ["bar"] ["-+"; "foo"; ""]
#else
  skip_if true "OCaml < 4.12 doesn't have Arg.Rest_all"
#endif

let test_expand _ =
  assert_complete ["bar"; "baz"] ["--expand"; "b"];
  assert_complete all_empty ["--expand"; "b"; ""]

let test_side_effect _ =
  assert_complete [] ["--side_effect"; ""];
  assert_complete ["foo"] ["--side_effect"; "foo"; ""];
  assert_complete all_empty ["--side_effect"; "foo"; "foo"; ""]

let test_help _ =
  assert_complete ["--help"] ["--h"];
  assert_complete ["-help"] ["-h"]

let test_skip _ =
  assert_complete ["--unit"] ["--foo"; "--uni"]


let tests =
  "arg_complete_test" >::: [
    "anon" >:: test_anon;
    "key" >:: test_key;
    "unit" >:: test_unit;
    "bool" >:: test_bool;
    "set" >:: test_set;
    "clear" >:: test_clear;
    "string" >:: test_string;
    "set_string" >:: test_set_string;
    "symbol" >:: test_symbol;
    "tuple" >:: test_tuple;
    "rest" >:: test_rest;
    "rest_all" >:: test_rest_all;
    "expand" >:: test_expand;
    "side_effect" >:: test_side_effect;
    "help" >:: test_help;
    "skip" >:: test_skip;
  ]

let () = run_test_tt_main tests
