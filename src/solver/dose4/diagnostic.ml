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

type reason_int =
  | DependencyInt of (int * Cudf_types.vpkg list * int list)
  | MissingInt of (int * Cudf_types.vpkg list)
  | ConflictInt of (int * int * Cudf_types.vpkg)

type result_int =
  | SuccessInt of (?all:bool -> unit -> int list)
  | FailureInt of (unit -> reason_int list)

type request_int = int list

(** One un-installability reason for a package *)
type reason =
  | Dependency of (Cudf.package * Cudf_types.vpkg list * Cudf.package list)
      (** Not strictly a un-installability, Dependency (a,vpkglist,pkglist) is used
      to recontruct the the dependency path from the root package to the
      offending un-installable package *)
  | Missing of (Cudf.package * Cudf_types.vpkg list)
      (** Missing (a,vpkglist) means that the dependency
      [vpkglist] of package [a] cannot be satisfied *)
  | Conflict of (Cudf.package * Cudf.package * Cudf_types.vpkg)
      (** Conflict (a,b,vpkg) means that the package [a] is in conflict
      with package [b] because of vpkg *)

(** The request provided to the solver.
    Check the installability of one package or the
    coinstallability of a list of packages *)
type request = Cudf.package list

(** The result of an installability query *)
type result =
  | Success of (?all:bool -> unit -> Cudf.package list)
      (** If successfull returns a function that will
      return the installation set for the given query. Since
      not all packages are tested for installability directly, the
      installation set might be empty. In this case, the solver can
      be called again to provide the real installation set
      using the parameter [~all:true] *)
  | Failure of (unit -> reason list)
      (** If unsuccessful returns a function containing the list of reason *)

type diagnosis = { result : result; request : request }

let reason map universe =
  let from_sat = CudfAdd.inttopkg universe in
  let globalid = map#vartoint (Cudf.universe_size universe) in
  List.filter_map (function
      | DependencyInt (i, _vl, _il) when i = globalid -> None
      | MissingInt (i, _vl) when i = globalid ->
          Util.fatal
            "the package encoding global constraints can't be missing (uid %d)"
            i
      | ConflictInt (i, j, _vpkg) when i = globalid || j = globalid ->
          Util.fatal
            "the package encoding global constraints can't be in conflict (uid \
             %d - %d)"
            i
            j
      | DependencyInt (i, vl, il) ->
          Some
            (Dependency
               ( from_sat (map#inttovar i),
                 vl,
                 List.map (fun i -> from_sat (map#inttovar i)) il ))
      | MissingInt (i, vl) -> Some (Missing (from_sat (map#inttovar i), vl))
      | ConflictInt (i, j, vpkg) ->
          Some
            (Conflict
               (from_sat (map#inttovar i), from_sat (map#inttovar j), vpkg)))

let result map universe result =
  let from_sat = CudfAdd.inttopkg universe in
  let globalid = map#vartoint (Cudf.universe_size universe) in
  match result with
  | SuccessInt f_int ->
      Success
        (fun ?(all = false) () ->
          List.filter_map
            (function
              | i when i = globalid -> None
              | i ->
                  Some
                    { (from_sat (map#inttovar i)) with Cudf.installed = true })
            (f_int ~all ()))
  | FailureInt f -> Failure (fun () -> reason map universe (f ()))

let request universe result = List.map (CudfAdd.inttopkg universe) result

(* XXX here the threatment of result and request is not uniform.
 * On one hand indexes in result must be processed with map#inttovar
 * as they represent indexes associated with the solver.
 * On the other hand the indexes in result represent cudf uid and
 * therefore do not need to be processed.
 * Ideally the compiler should make sure that we use the correct indexes
 * but we should annotate everything making packing/unpackaing handling
 * a bit too heavy *)
let diagnosis map universe res req =
  let result = result map universe res in
  let request = request universe req in
  { result; request }

module ResultHash = Hashtbl.Make (struct
  type t = reason

  let equal v w =
    match (v, w) with
    | (Missing (_, v1), Missing (_, v2)) -> v1 = v2
    | (Conflict (i1, j1, _), Conflict (i2, j2, _)) -> i1 = i2 && j1 = j2
    | _ -> false

  let hash = function
    | Missing (_, vpkgs) -> Hashtbl.hash vpkgs
    | Conflict (i, j, _) -> Hashtbl.hash (i, j)
    | _ -> assert false
end)

(* XXX unplug your imperative brain and rewrite this as a tail recoursive
 * function ! *)
let minimize roots l =
  let module H = Hashtbl in
  let h = H.create (List.length l) in
  List.iter (fun p -> H.add h p.Cudf.package p) l ;
  let acc = H.create 1023 in
  let rec visit pkg =
    if not (H.mem acc pkg) then (
      H.add acc pkg () ;
      List.iter
        (fun vpkgformula ->
          List.iter
            (fun (name, constr) ->
              try
                let p = H.find h name in
                if Cudf.version_matches p.Cudf.version constr then visit p
              with Not_found -> ())
            vpkgformula)
        pkg.Cudf.depends)
  in
  (match roots with [r] -> visit r | _rl -> List.iter visit l) ;
  H.fold (fun k _ l -> k :: l) acc []

let get_installationset ?(minimal = false) = function
  | { result = Success f; request = req } ->
      let s = f ~all:true () in
      if minimal then minimize req s else s
  | { result = Failure _; _ } -> raise Not_found

let is_solution = function
  | { result = Success _; _ } -> true
  | { result = Failure _; _ } -> false
