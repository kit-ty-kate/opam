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

(** Dependency solver. Implementation of the Edos algorithms *)

(** the solver is an abstract data type associated to a universe *)
type solver

(** check if the given package can be installed in the universe

    Packages marked as `Keep_package must be always installed.*)
val edos_install :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
  Cudf.universe ->
  Cudf.package ->
  Diagnostic.diagnosis

(** check if the give package list can be installed in the universe  *)
val edos_coinstall :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
  Cudf.universe ->
  Cudf.package list ->
  Diagnostic.diagnosis

(** [listcheck ~callback:c subuniverse l] check if all packages in [l] can be
   installed.

   Invariant : l is a subset of universe can be installed in the solver universe.

   It is responsability of the user to pass listcheck an appropriate subuniverse`

   @param callback : execute a function for each package.
   @return the number of broken packages
 *)
val listcheck :
  ?global_constraints:(Cudf_types.vpkglist * Cudf.package list) list ->
  ?callback:(Diagnostic.diagnosis -> unit) ->
  ?explain:bool ->
  Cudf.universe ->
  Cudf.package list ->
  int

type enc = Cnf | Dimacs

(** The result of the depclean function is a tuple containing a package, a list
    of dependencies that are redundant and a list of conflicts that are redundant *)
type depclean_result =
  Cudf.package
  * (Cudf_types.vpkglist * Cudf_types.vpkg * Cudf.package list) list
  * (Cudf_types.vpkg * Cudf.package list) list

type solver_result =
  | Sat of (Cudf.preamble option * Cudf.universe)
  | Unsat of Diagnostic.diagnosis option
  | Error of string

(** an empty package used to enforce global contraints on the request *)
val dummy_request : Cudf.package

(** [check_request] check if there exists a solution for the give cudf document
    if ?dummy is specified, adds this dummy package to the user request. This parameter
    is used to encode a list of 'essential' packages that must always be installed in
    the solution alongside with the user request.
    if ?criteria is specified it will be used as optimization criteria.
    if ?explain is specified and there is no solution for the give request, the
    result will contain the failure reason. *)
val check_request :
  ?criteria:string ->
  ?dummy:Cudf.package ->
  ?explain:bool ->
  Cudf.cudf ->
  solver_result

(** Same as [check_request], but allows to specify any function to call the
    external solver. It should raise [Depsolver.Unsat] on failure. *)
val check_request_using :
  ?call_solver:(Cudf.cudf -> Cudf.preamble option * Cudf.universe) ->
  ?dummy:Cudf.package ->
  ?explain:bool ->
  Cudf.cudf ->
  solver_result
