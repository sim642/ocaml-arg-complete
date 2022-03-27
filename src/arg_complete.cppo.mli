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

module Rest_all_compat: sig
  type t
  val create: (string list -> unit) -> complete_all -> t
  val spec: t -> spec
  val finish: t -> unit
end

type arg_speclist = (Arg.key * Arg.spec * Arg.doc) list
type speclist = (Arg.key * spec * Arg.doc) list

val arg_spec: spec -> Arg.spec
val arg_speclist: speclist -> arg_speclist

val complete_argv: string list -> speclist -> complete -> string list


val empty: complete
val empty_all: complete_all
val strings: string list -> complete

module Util: sig
  val starts_with: prefix:string -> string -> bool
end
