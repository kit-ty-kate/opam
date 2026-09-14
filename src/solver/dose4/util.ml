(*****************************************************************************)
(*  Copyright (C) 2009  <pietro.abate@pps.jussieu.fr>                        *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

type label = string

(* ExtList.remove_if *)
let rec list_remove_if f = function
  | [] -> []
  | x::xs when f x -> xs
  | x::xs -> x :: list_remove_if f xs

let fatal fmt =
  Printf.ksprintf
    (fun s ->
       Printf.eprintf "FATAL ERROR: %s\n%!" s ;
       Stdlib.exit 64)
    fmt

module StringHashtbl = Hashtbl.Make (struct
  type t = string

  let equal (a : string) (b : string) = a = b

  let hash s = Hashtbl.hash s
end)

module StringPairHashtbl = Hashtbl.Make (struct
  type t = string * string

  let equal (a : string * string) (b : string * string) = a = b

  let hash s = Hashtbl.hash s
end)

module IntHashtbl = Hashtbl.Make (struct
  type t = int

  let equal (a : int) (b : int) = a = b

  let hash i = Hashtbl.hash i
end)

module IntPairHashtbl = Hashtbl.Make (struct
  type t = int * int

  let equal (a : int * int) (b : int * int) = a = b

  let hash i = Hashtbl.hash i
end)

class type projection =
  object
    method add : int -> unit

    method inttovar : int -> int

    method vartoint : int -> int
  end

(** associate a sat solver variable to a package id *)
class intprojection size =
  object
    val vartoint = IntHashtbl.create (2 * size)

    val inttovar = Array.make size 0

    val mutable counter = 0

    (** add a package id to the map *)
    method add v =
      if size = 0 then assert false ;
      if counter > size - 1 then assert false ;
      IntHashtbl.add vartoint v counter ;
      inttovar.(counter) <- v ;
      counter <- counter + 1

    (** given a package id return a sat solver variable
      raise Not_found if the package id is not known *)
    method vartoint v = IntHashtbl.find vartoint v

    (* given a sat solver variable return a package id *)
    method inttovar i =
      if i >= size then fatal "out of boundary i = %d size = %d" i size ;
      inttovar.(i)
  end

class identity =
  object
    method add (_ : int) = ()
    method vartoint (v : int) = v
    method inttovar (v : int) = v
  end
