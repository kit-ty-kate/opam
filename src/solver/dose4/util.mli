(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

(** Debug, ProgressBars, Timers and Loggers *)

type label = string

val list_remove_if : ('a -> bool) -> 'a list -> 'a list

val fatal : ('a, unit, label, 'b) format4 -> 'a

module IntHashtbl : Hashtbl.S with type key = int

module IntPairHashtbl : Hashtbl.S with type key = int * int

module StringHashtbl : Hashtbl.S with type key = string

module StringPairHashtbl : Hashtbl.S with type key = string * string

(** associate a sat solver variable to a package id *)
class type projection =
  object
    (** add a package id to the map *)
    method add : int -> unit

    (** given a package id return a sat solver variable
        raise Not_found if the package id is not known *)
    method inttovar : int -> int

    (** given a sat solver variable return a package id *)
    method vartoint : int -> int
  end

(** identity projection *)
class identity : projection

(** [intprojection n] integer projection of size [n] *)
class intprojection : int -> projection
