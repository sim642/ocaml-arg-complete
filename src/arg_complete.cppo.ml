type complete = string -> string list
type complete_all = string list -> string list

type spec =
  | Unit of (unit -> unit)
  | Bool of (bool -> unit)
  | Set of bool ref
  | Clear of bool ref
  | String of (string -> unit) * complete
  | Set_string of string ref * complete
  | Int of (int -> unit) * complete
  | Set_int of int ref * complete
  | Float of (float -> unit) * complete
  | Set_float of float ref * complete
  | Symbol of string list * (string -> unit)
  | Tuple of spec list
  | Rest of (string -> unit) * complete
#if OCAML_VERSION >= (4, 12, 0)
  | Rest_all of (string list -> unit) * complete_all
#endif
#if OCAML_VERSION >= (4, 5, 0)
  | Expand of (string -> string array)
#endif

type arg_speclist = (Arg.key * Arg.spec * Arg.doc) list
type speclist = (Arg.key * spec * Arg.doc) list

let rec arg_spec: spec -> Arg.spec = function
  | Unit f -> Arg.Unit f
  | Bool f -> Arg.Bool f
  | Set r -> Arg.Set r
  | Clear r -> Arg.Clear r
  | String (f, _c) -> Arg.String f
  | Set_string (r, _c) -> Arg.Set_string r
  | Int (f, _c) -> Arg.Int f
  | Set_int (r, _c) -> Arg.Set_int r
  | Float (f, _c) -> Arg.Float f
  | Set_float (r, _c) -> Arg.Set_float r
  | Symbol (l, f) -> Arg.Symbol (l, f)
  | Tuple l -> Arg.Tuple (List.map arg_spec l)
  | Rest (f, _c) -> Arg.Rest f
#if OCAML_VERSION >= (4, 12, 0)
  | Rest_all (f, _c) -> Arg.Rest_all f
#endif
#if OCAML_VERSION >= (4, 5, 0)
  | Expand f -> Arg.Expand f
#endif

let arg_speclist: speclist -> arg_speclist = fun l ->
  List.map (fun (k, sc, d) -> (k, arg_spec sc, d)) l

#if OCAML_VERSION >= (4, 13, 0)
let starts_with = String.starts_with
#else
(* Copied from OCaml Stdlib *)
let starts_with ~prefix s =
  let len_s = String.length s
  and len_pre = String.length prefix in
  let rec aux i =
    if i = len_pre then true
    else if String.unsafe_get s i <> String.unsafe_get prefix i then false
    else aux (i + 1)
  in len_s >= len_pre && aux 0
#endif

let complete_strings l s =
  List.filter (starts_with ~prefix:s) l

#if OCAML_VERSION >= (4, 8, 0)
let filter_map = List.filter_map
#else
(* Copied from OCaml Stdlib *)
let filter_map f =
  let rec aux accu = function
    | [] -> List.rev accu
    | x :: l ->
        match f x with
        | None -> aux accu l
        | Some v -> aux (v :: accu) l
  in
  aux []
#endif

let complete_argv (argv: string list) (speclist: speclist) (anon_complete: complete): string list =
  let rec complete_arg (argv: string list) =
    match argv with
    | [] -> []
    | arg :: argv' ->
      try
        let (key, spec, _) = List.find (fun (key, _, _) -> arg = key) speclist in
        let rec complete_spec spec argv' =
          match spec, argv' with
          | Unit _f, argv' -> complete_arg argv'
          | Bool _f, [arg'] -> complete_strings ["false"; "true"] arg'
          | Bool _f, _ :: argv' -> complete_arg argv'
          | Set _r, argv' -> complete_arg argv'
          | Clear _r, argv' -> complete_arg argv'
          | String (_f, c), [arg'] -> c arg'
          | String (_f, c), arg' :: argv' -> ignore (c arg'); complete_arg argv'
          | Set_string (_r, c), [arg'] -> c arg'
          | Set_string (_r, c), arg' :: argv' -> ignore (c arg'); complete_arg argv'
          | Int (_f, c), [arg'] -> c arg'
          | Int (_f, c), arg' :: argv' -> ignore (c arg'); complete_arg argv'
          | Set_int (_r, c), [arg'] -> c arg'
          | Set_int (_r, c), arg' :: argv' -> ignore (c arg'); complete_arg argv'
          | Float (_f, c), [arg'] -> c arg'
          | Float (_f, c), arg' :: argv' -> ignore (c arg'); complete_arg argv'
          | Set_float (_r, c), [arg'] -> c arg'
          | Set_float (_r, c), arg' :: argv' -> ignore (c arg'); complete_arg argv'
          | Tuple l, argv' ->
            let rec complete_tuple l argv' = match l, argv' with
              | s :: _, [arg'] -> complete_spec s [arg']
              | s :: l', arg' :: argv' -> ignore (complete_spec s [arg']); complete_tuple l' argv'
              | [], argv' -> complete_arg argv'
              | _, _ -> failwith "cannot complete tuple"
            in
            complete_tuple l argv'
          | Symbol (l, _f), [arg'] -> complete_strings l arg'
          | Symbol (_l, _f), _ :: argv' -> complete_arg argv'
          | Rest (_f, c), argv' ->
            let rec complete_rest = function
              | [arg] -> c arg
              | arg :: argv' -> ignore (c arg); complete_rest argv'
              | _ -> failwith "cannot complete rest"
            in
            complete_rest argv'
#if OCAML_VERSION >= (4, 12, 0)
          | Rest_all (_f, c), argv' -> c argv'
#endif
#if OCAML_VERSION >= (4, 5, 0)
          | Expand f, arg' :: argv' -> complete_arg (Array.to_list (f arg') @ argv')
#endif
          | _, _ -> failwith "cannot complete"
        in
        if argv' = [] then
          [key] (* complete key itself *)
        else
          complete_spec spec argv'
      with Not_found ->
        if argv' = [] then
          if String.length arg = 0 || arg.[0] <> '-' then
            anon_complete arg
          else
            filter_map (fun (key, _spec, _doc) ->
                if starts_with ~prefix:arg key then
                  Some key
                else
                  None
              ) speclist
        else
          complete_arg argv'
  in
  complete_arg argv


module Rest_all_compat =
struct
#if OCAML_VERSION >= (4, 12, 0)
  type t = {
    parse: string list -> unit;
    complete: complete_all;
  }

  let create parse complete = {parse; complete}

  let spec {parse; complete; _} =
    Rest_all (parse, complete)

  let finish _ = ()
#else
  type t = {
    parse: string list -> unit;
    complete: complete_all; (* TODO: use *)
    mutable parse_args: string list option;
    mutable complete_args: string list option; (* TODO: use *)
  }

  let create parse complete =
    {parse; complete; parse_args = None; complete_args = None}

  let spec r =
    let cons_opt x xs = Some (match xs with
        | None -> [x]
        | Some xs -> x :: xs
      )
    in
    let parse s = r.parse_args <- cons_opt s r.parse_args in
    let complete s = r.complete_args <- cons_opt s r.complete_args; [] in
    Rest (parse, complete)

  let finish {parse; parse_args; _} =
    match parse_args with
    | Some acc -> parse (List.rev acc)
    | None -> ()
#endif
end
