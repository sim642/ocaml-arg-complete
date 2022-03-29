(** {1 Specification}
    Command line argument specifications are extended with completion functions. *)

type complete = string -> string list
(** Type for function providing single-argument completions. *)

type complete_all = string list -> string list
(** Type for function providing multiple-argument completions (only in {!Rest_all}). *)

type spec =
  | Unit of (unit -> unit) (** Call the function with unit argument *)
  | Bool of (bool -> unit) (** Call the function with a bool argument *)
  | Set of bool ref (** Set the reference to true *)
  | Clear of bool ref (** Set the reference to false *)
  | String of (string -> unit) * complete (** Call the function with a string argument *)
  | Set_string of string ref * complete (** Set the reference to the string argument *)
  | Int of (int -> unit) * complete (** Call the function with an int argument *)
  | Set_int of int ref * complete (** Set the reference to the int argument *)
  | Float of (float -> unit) * complete (** Call the function with a float argument *)
  | Set_float of float ref * complete (** Set the reference to the float argument *)
  | Tuple of spec list (** Take several arguments according to the spec list *)
  | Symbol of string list * (string -> unit) (** Take one of the symbols as argument and call the function with the symbol *)
  | Rest of (string -> unit) * complete (** Stop interpreting keywords and call the function with each remaining argument *)
#if OCAML_VERSION >= (4, 12, 0)
  | Rest_all of (string list -> unit) * complete_all
  (** Stop interpreting keywords and call the function with all remaining arguments
      @since OCaml 4.12 *)
#endif
#if OCAML_VERSION >= (4, 5, 0)
  | Expand of (string -> string array)
  (** If the remaining arguments to process are of the form [["-foo"; "arg"] @ rest] where "foo" is registered as [Expand f], then the arguments [f "arg" @ rest] are processed. Only allowed in {!value:Stdlib.Arg.parse_and_expand_argv_dynamic}.
      @since OCaml 4.05 *)
#endif
(** Command line argument specification like {!Stdlib.Arg.spec}, but extended with completion functions. *)

(** Compatibility for {!Rest_all} before OCaml 4.12 *)
module Rest_all_compat: sig
  type t
  val create: (string list -> unit) -> complete_all -> t
  val spec: t -> spec
  val finish: t -> unit
end

type speclist = (Arg.key * spec * Arg.doc) list
type arg_speclist = (Arg.key * Arg.spec * Arg.doc) list

(** {1 Parsing}
    The library does not provide parsing of command line arguments itself.
    Instead, the following functions can be used to strip completion functions from specifications, such that parsing functions from {!Stdlib.Arg} can be used. *)

val arg_spec: spec -> Arg.spec
(** Strip completion functions from {!spec}. *)

val arg_speclist: speclist -> arg_speclist
(** Strip completion functions from {!speclist}. *)

(** {1 Completing} *)

type anon_complete = complete

val complete_argv: string list -> speclist -> anon_complete -> string list
(** [complete_argv args speclist anon_complete] provides the completions for the partial arguments [args] using [speclist] for options and [anon_complete] for anonymous arguments. *)

val complete_argv2: string list -> speclist -> anon_complete -> string list

(** {1 Convenience} *)

val empty: complete
(** Completion function with constant empty result. *)

val empty_all: complete_all
(** Completion function with constant empty result for {!Rest_all}. *)

val strings: string list -> complete
(** Completion function for a list of possible strings. *)

module Util: sig
  val starts_with: prefix:string -> string -> bool
  (** [starts_with ~prefix s] is [true] if and only if [s] starts with [prefix].

      Provided for compatibility before OCaml 4.13. *)
end
