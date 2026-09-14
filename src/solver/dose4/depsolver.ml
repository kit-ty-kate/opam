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

type solver = Depsolver_int.solver

(** [listcheck ?callback universe pkglist] check if a subset of packages
    un the universe are installable.

    @param pkglist list of packages to be checked
    @return the number of packages that cannot be installed
*)
let listcheck ?(global_constraints = []) ?callback ?(explain = true) universe
    pkglist =
  let aux ?callback univ idlist =
    let global_constraints =
      List.map
        (fun (vpkg, l) -> (vpkg, List.map (CudfAdd.pkgtoint universe) l))
        global_constraints
    in
    let solver =
      Depsolver_int.init_solver_univ ~global_constraints ~explain univ
    in
    let failed = ref 0 in
    let size = Cudf.universe_size univ + 1 in
    let tested = Array.make size false in
    let check = Depsolver_int.pkgcheck callback explain solver tested in
    (match fst solver.Depsolver_int.globalid with
    | (false, false) ->
        List.iter (fun id -> if not (check id) then incr failed) idlist
    | _ ->
        let gid = snd solver.Depsolver_int.globalid in
        List.iter
          (function
            | id when id = gid -> () | id -> if not (check id) then incr failed)
          idlist) ;
    !failed
  in
  let idlist = List.map (CudfAdd.pkgtoint universe) pkglist in
  let map = new Util.identity in
  match callback with
  | None -> aux universe idlist
  | Some f ->
      let callback_int (res, req) =
        f (Diagnostic.diagnosis map universe res req)
      in
      aux ~callback:callback_int universe idlist

let edos_install_cache univ cudfpool pkglist =
  let idlist = List.map (CudfAdd.pkgtoint univ) pkglist in
  let closure = Depsolver_int.dependency_closure_cache cudfpool idlist in
  let solver =
    Depsolver_int.init_solver_closure ~global_constraints:[] cudfpool closure
  in
  let res = Depsolver_int.solve solver ~explain:true idlist in
  Diagnostic.diagnosis solver.Depsolver_int.map univ res idlist

let edos_install ?(global_constraints = []) universe pkg =
  let global_constraints =
    List.map
      (fun (vpkg, l) -> (vpkg, List.map (CudfAdd.pkgtoint universe) l))
      global_constraints
  in
  let cudfpool = Depsolver_int.init_pool_univ ~global_constraints universe in
  edos_install_cache universe cudfpool [pkg]

let edos_coinstall ?(global_constraints = []) universe pkglist =
  let global_constraints =
    List.map
      (fun (vpkg, l) -> (vpkg, List.map (CudfAdd.pkgtoint universe) l))
      global_constraints
  in
  let cudfpool = Depsolver_int.init_pool_univ ~global_constraints universe in
  edos_install_cache universe cudfpool pkglist

type enc = Cnf | Dimacs

type solver_result =
  | Sat of (Cudf.preamble option * Cudf.universe)
  | Unsat of Diagnostic.diagnosis option
  | Error of string

let dummy_request =
  { Cudf.default_package with Cudf.package = "dose-dummy-request"; version = 1 }

(* add a version constraint to ensure name is upgraded *)
let upgrade_constr universe name =
  match Cudf.get_installed universe name with
  | [] -> (name, None)
  | [p] -> (name, Some (`Geq, p.Cudf.version))
  | pl ->
      let p = List.hd (List.sort Cudf.( >% ) pl) in
      (name, Some (`Geq, p.Cudf.version))

let add_dummy universe request dummy =
  let deps =
    let il = request.Cudf.install in
    (* we preserve the user defined constraints, while adding the upgrade constraint *)
    let ulc =
      List.filter
        (function (_, Some _) -> true | _ -> false)
        request.Cudf.upgrade
    in
    let ulnc =
      List.map
        (fun (name, _) -> upgrade_constr universe name)
        request.Cudf.upgrade
    in
    let l = il @ ulc @ ulnc in
    List.map (fun j -> [j]) l
  in
  let dummy =
    { dummy with
      Cudf.depends = deps @ dummy.Cudf.depends;
      conflicts = request.Cudf.remove @ dummy.Cudf.conflicts
    }
  in
  (* XXX it should be possible to add a package to a cudf document ! *)
  let pkglist = Cudf.get_packages universe in
  let universe = Cudf.load_universe (dummy :: pkglist) in
  (universe, dummy)

let remove_dummy ~explain pre (dummy, d) =
  if Diagnostic.is_solution d then
    let is =
      Util.list_remove_if (Cudf.( =% ) dummy) (Diagnostic.get_installationset d)
    in
    Sat (Some pre, Cudf.load_universe is)
  else if explain then Unsat (Some d)
  else Unsat None

let check_request_using ?call_solver ?dummy ?(explain = false)
    (pre, universe, request) =
  match (call_solver, dummy) with
  | (None, None) ->
      let (u, r) = add_dummy universe request dummy_request in
      remove_dummy ~explain pre (r, edos_install u r)
  | (None, Some dummy) ->
      let (u, r) = add_dummy universe request dummy in
      remove_dummy ~explain pre (r, edos_install u r)
  | (Some call_solver, None) -> (
      try
        let (presol, sol) = call_solver (pre, universe, request) in
        Sat (presol, sol)
      with
      | CudfSolver.Unsat when not explain -> Unsat None
      | CudfSolver.Unsat when explain ->
          let (u, r) = add_dummy universe request dummy_request in
          remove_dummy ~explain pre (r, edos_install u r))
  | (Some call_solver, Some dummy) -> (
      let (u, dr) = add_dummy universe request dummy in
      let dr_constr = (dr.Cudf.package, Some (`Eq, dr.Cudf.version)) in
      let r =
        { request with Cudf.install = dr_constr :: request.Cudf.install }
      in
      try
        let (presol, sol) = call_solver (pre, u, r) in
        let is = Util.list_remove_if (Cudf.( =% ) dr) (Cudf.get_packages sol) in
        Sat (presol, Cudf.load_universe is)
      with
      | CudfSolver.Unsat when not explain -> Unsat None
      | CudfSolver.Unsat when explain ->
          let (u, r) = add_dummy universe request dummy in
          remove_dummy ~explain pre (r, edos_install u r)
      | CudfSolver.Error s -> Error s)

(** check if a cudf request is satisfiable. we do not care about
    universe consistency . We try to install a dummy package *)
let check_request ?criteria ?dummy ?explain cudf =
  check_request_using ?dummy ?explain cudf

type depclean_result =
  Cudf.package
  * (Cudf_types.vpkglist * Cudf_types.vpkg * Cudf.package list) list
  * (Cudf_types.vpkg * Cudf.package list) list
