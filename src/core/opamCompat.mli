(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module String = String

module Char = Char

module Either
#if OCAML_VERSION >= (4, 12, 0)
= Either
#else
: sig
  type ('a, 'b) t =
  | Left of 'a
  | Right of 'b
end
#endif

module Int
#if OCAML_VERSION >= (4, 12, 0)
= Int
#else
: sig
  val compare: int -> int -> int
end
#endif

module Printexc
#if OCAML_VERSION >= (4, 5, 0)
= Printexc
#else
: sig
  include module type of struct include Printexc end

  val raise_with_backtrace: exn -> raw_backtrace -> 'a
end
#endif

module Unix
#if OCAML_VERSION >= (4, 6, 0)
= Unix
#else
: sig
  include module type of struct include Unix end

  val map_file : Unix.file_descr -> ?pos:int64 -> ('a, 'b) Bigarray.kind ->
                 'c Bigarray.layout -> bool -> int array ->
                 ('a, 'b, 'c) Bigarray.Genarray.t
end
#endif

module Uchar = Uchar

module Buffer
#if OCAML_VERSION >= (4, 6, 0)
= Buffer
#else
: sig
  include module type of struct include Buffer end

  val add_utf_8_uchar : t -> Uchar.t -> unit
end
#endif

module Filename
#if OCAML_VERSION >= (4, 4, 0)
= Filename
#else
: sig
  include module type of struct include Filename end

  val extension : string -> string
end
#endif

module Result
#if OCAML_VERSION >= (4, 8, 0)
= Result
#else
: sig
  type ('a, 'e) t
    = ('a, 'e) result
    = Ok of 'a | Error of 'e
end
#endif

module List
#if OCAML_VERSION >= (4, 10, 0)
= List
#else
: sig
  include module type of List

  val concat_map : ('a -> 'b list) -> 'a list -> 'b list
end
#endif

#if OCAML_VERSION < (4, 7, 0)
module Stdlib = Pervasives
#else
module Stdlib = Stdlib
#endif
