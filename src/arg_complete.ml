type complete = string -> string list

type spec =
  | Unit of (unit -> unit)
  | String of (string -> unit) * complete

type arg_speclist = (Arg.key * Arg.spec * Arg.doc) list
type speclist = (Arg.key * spec * Arg.doc) list

let arg_spec: spec -> Arg.spec = function
  | Unit f -> Arg.Unit f
  | String (f, _c) -> Arg.String f

let arg_speclist: speclist -> arg_speclist = fun l ->
  List.map (fun (k, sc, d) -> (k, arg_spec sc, d)) l

let complete_argv (argv: string array) (speclist: speclist) (_anon_complete: complete): string list =
  let rec complete_arg (argv: string list) =
    match argv with
    | [] -> []
    | arg :: argv' ->
      List.concat_map (fun (key, spec, _doc) ->
          if arg = key then (
            match spec, argv' with
            | Unit _f, argv' -> complete_arg argv'
            | String (_f, c), arg' :: _ -> c arg'
            | _, _ -> []
          )
          else if String.starts_with ~prefix:arg key then
            [key]
          else
            []
        ) speclist
  in
  complete_arg (List.tl (Array.to_list argv))
