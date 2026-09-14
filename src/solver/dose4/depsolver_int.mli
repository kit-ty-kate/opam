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

(** Dependency solver. Low Level API *)

(** Implementation of the EDOS algorithms (and more).
    This module respects the cudf semantic.

    This module contains two type of functions.
    Normal functions work on a cudf universe. These are just a wrapper to
    _cache functions.

    _cache functions work on a pool of ids that is a more compact
    representation of a cudf universe based on arrays of integers.
    _cache function can be used to avoid recreating the pool for each
    operation and therefore speed up operations.
*)

(** Sat Solver instance *)
module R : sig
  type reason = Diagnostic.reason_int
end

module S : EdosSolver.T with module X = R

(** internal state of the sat solver. The map allows to transform
    sat solver variables (that must be contiguous) to integers
    representing the id of a package *)
type solver =
  { constraints : S.state;  (** the sat problem *)
    map : Util.projection;
        (** a map from cudf package ids to solver ids *)
    globalid : (bool * bool) * int
        (** (keep_constrains,global_constrains),gui) where
                                     gid is the last index of the cudfpool. Used to encode
                                     a 'dummy' package and to enforce global constraints.
                                     keep_constrains and global_constrains are true if either
                                     keep_constrains or global_constrains are enforceble.
                                  *)
  }

type global_constraints = (Cudf_types.vpkglist * int list) list

(** Solver Package Pool. [pool_t] is an array where each index
  is an solver variable and the content of the array associates
  cudf dependencies to a list of solver varialbles representing
  a package *)
type dep_t =
  (Cudf_types.vpkg list * int list) list * (Cudf_types.vpkg * int list) list

and pool = dep_t array

(** A pool can either be a low level representation of the universe
    where all integers are interpreted as solver variables or a universe
    where all integers are interpreted as cudf package indentifiers. The
    boolean associate to the cudfpool is true if keep_constrains are
    present in the universe. The last index of the pool is the globalid *)
and t = [ `SolverPool of pool | `CudfPool of bool * pool ]

(** Given a cudf universe , this function returns a [CudfPool].
    We assume that cudf uid are sequential and we can use them as an array index.
    The last index of the pool is the globalid.
 *)
val init_pool_univ :
  global_constraints:global_constraints ->
  Cudf.universe ->
  [> `CudfPool of bool * pool ]

(** Call the sat solver

    @param tested: optional int array used to cache older results
    @param explain: if try we add all the information needed to create the
                    explanation graph
*)
val solve :
  ?tested:bool array ->
  explain:bool ->
  solver ->
  Diagnostic.request_int ->
  Diagnostic.result_int

(** [pkgcheck callback solver tested id].
   This function is used to "distcheck" a list of packages
   *)
val pkgcheck :
  (Diagnostic.result_int * Diagnostic.request_int -> unit) ->
  solver ->
  bool array ->
  int ->
  bool

(** Constraint solver initialization

    @param buffer debug buffer to print out debug messages
    @param univ cudf package universe
*)
val init_solver_univ :
  global_constraints:global_constraints ->
  ?buffer:bool ->
  Cudf.universe ->
  solver

(* pool = cudf pool - closure = dependency clousure . cudf uid list *)

(** Constraint solver initialization

    @param buffer debug buffer to print out debug messages
    @param pool dependencies and conflicts array idexed by package id
    @param closure subset of packages used to initialize the solver
*)
val init_solver_closure :
  global_constraints:global_constraints ->
  ?buffer:bool ->
  [< `CudfPool of bool * pool ] ->
  int list ->
  solver

(** [dependency_closure_cache pool l] return the union of the dependency closure of
    all packages in [l] in the given pool of packages. The result always contains the
    globalid.

    @param maxdepth the maximum cone depth (infinite by default)
    @param conjunctive consider only conjunctive dependencies (false by default)
*)
val dependency_closure_cache :
  ?maxdepth:int ->
  ?conjunctive:bool ->
  [< `CudfPool of bool * pool ] ->
  int list ->
  int list
