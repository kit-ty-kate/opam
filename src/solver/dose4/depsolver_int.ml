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

module R = struct
  type reason = Diagnostic.reason_int
end

module S = EdosSolver.M (R)

type solver =
  { constraints : S.state;
    map : Util.projection;
    globalid : (bool * bool) * int
  }

type dep_t =
  (Cudf_types.vpkg list * S.var list) list * (Cudf_types.vpkg * S.var list) list

and pool = dep_t array

and t = [ `SolverPool of pool | `CudfPool of bool * pool ]

(* cudf uid -> cudf uid array . Here we assume cudf uid are sequential
   and we can use them as an array index *)
let init_pool_univ univ =
  (* the last element of the array *)
  let size = Cudf.universe_size univ in
  let keep = Hashtbl.create 200 in
  let pool =
    (* here I initalize the pool to size + 1, that is I reserve one spot
     * to encode the global constraints associated with the universe.
     * However, since they are global, I've to add the at the end, after
     * I have analyzed all packages in the universe. *)
    Array.init (size + 1) (fun uid ->
        try
          if uid = size then ([], []) (* the last index *)
          else
            let pkg = Cudf.package_by_uid univ uid in
            let dll =
              List.map
                (fun vpkgs -> (vpkgs, CudfAdd.resolve_vpkgs_int univ vpkgs))
                pkg.Cudf.depends
            in
            let cl =
              List.filter_map
                (fun vpkg ->
                  match CudfAdd.resolve_vpkg_int univ vpkg with
                  | [] -> None
                  | l -> Some (vpkg, l))
                pkg.Cudf.conflicts
            in
            (if pkg.Cudf.installed then
             match pkg.Cudf.keep with
             | `Keep_none -> ()
             | `Keep_package ->
                 List.iter
                   (fun id ->
                     CudfAdd.add_to_package_list
                       keep
                       (pkg.Cudf.package, None)
                       id)
                   (CudfAdd.resolve_vpkg_int univ (pkg.Cudf.package, None))
             | `Keep_version ->
                 CudfAdd.add_to_package_list
                   keep
                   (pkg.Cudf.package, Some (`Eq, pkg.Cudf.version))
                   uid
             | `Keep_feature ->
                 List.iter
                   (function
                     | (name, None) ->
                         List.iter
                           (fun id ->
                             CudfAdd.add_to_package_list keep (name, None) id)
                           (CudfAdd.resolve_vpkg_int univ (name, None))
                     | (name, Some (`Eq, v)) ->
                         List.iter
                           (fun id ->
                             CudfAdd.add_to_package_list
                               keep
                               (name, Some (`Eq, v))
                               id)
                           (CudfAdd.resolve_vpkg_int univ (name, Some (`Eq, v))))
                   pkg.Cudf.provides) ;
            (dll, cl)
        with Not_found ->
          Util.fatal
            "Package uid (%d) not found during solver pool initialization. \
             Packages uid must have no gaps in the given universe"
            uid)
  in
  let keep_dll =
    Hashtbl.fold
      (fun cnstr { contents = l } acc -> ([cnstr], l) :: acc)
      keep []
  in
  pool.(size) <- (keep_dll, []) ;
  `CudfPool (keep_dll <> [], pool)

(** this function creates an array indexed by solver ids that can be
    used to init the edos solver *)
let init_solver_pool map (`CudfPool (_keep_constraints, cudfpool)) closure =
  let convert (dll, cl) =
    let sdll =
      List.map (fun (vpkgs, uidl) -> (vpkgs, List.map map#vartoint uidl)) dll
    in
    let scl =
      (* ignore conflicts that are not in the closure.
       * if nobody depends on a conflict package, then it is irrelevant.
       * This requires a leap of faith in the user ability to build an
       * appropriate closure. If the closure is wrong, you are on your own *)
      List.map
        (fun (vpkg, uidl) ->
          let l =
            List.filter_map
              (fun uid ->
                try Some (map#vartoint uid)
                with Not_found -> None)
              uidl
          in
          (vpkg, l))
        cl
    in
    (sdll, scl)
  in
  let solverpool =
    Array.init (List.length closure) (fun sid ->
        convert cudfpool.(map#inttovar sid))
  in
  `SolverPool solverpool

(** initalise the sat solver. operate only on solver ids *)
let init_solver_cache ?(explain = true) (`SolverPool varpool)
    =
  let num_conflicts = ref 0 in
  let num_disjunctions = ref 0 in
  let num_dependencies = ref 0 in
  let if_explain l = if explain then l else [] in
  let varsize = Array.length varpool in
  let add_depend constraints vpkgs pkg_id l =
    let lit = S.lit_of_var pkg_id false in
    if List.length l = 0 then
      S.add_rule
        constraints
        [| lit |]
        (if_explain [Diagnostic.MissingInt (pkg_id, vpkgs)])
    else
      let lits = List.map (fun id -> S.lit_of_var id true) l in
      num_disjunctions := !num_disjunctions + List.length lits ;
      S.add_rule
        constraints
        (Array.of_list (lit :: lits))
        (if_explain [Diagnostic.DependencyInt (pkg_id, vpkgs, l)]) ;
      if List.length lits > 1 then
        S.associate_vars constraints (S.lit_of_var pkg_id true) l
  in
  let conflicts = Util.IntPairHashtbl.create (varsize / 10) in
  let add_conflict constraints vpkg (i, j) =
    if i <> j then
      let pair = (min i j, max i j) in
      (* we get rid of simmetric conflicts *)
      if not (Util.IntPairHashtbl.mem conflicts pair) then (
        incr num_conflicts ;
        Util.IntPairHashtbl.add conflicts pair () ;
        let p = S.lit_of_var i false in
        let q = S.lit_of_var j false in
        S.add_rule
          constraints
          [| p; q |]
          (if_explain [Diagnostic.ConflictInt (i, j, vpkg)]))
  in
  let exec_depends constraints pkg_id dll =
    List.iter
      (fun (vpkgs, dl) ->
        incr num_dependencies ;
        add_depend constraints vpkgs pkg_id dl)
      dll
  in
  let exec_conflicts constraints pkg_id cl =
    List.iter
      (fun (vpkg, l) ->
        List.iter (fun id -> add_conflict constraints vpkg (pkg_id, id)) l)
      cl
  in
  let constraints = S.initialize_problem varsize in
  Array.iteri
    (fun id (dll, cl) ->
      exec_depends constraints id dll ;
      exec_conflicts constraints id cl)
    varpool ;
  Util.IntPairHashtbl.clear conflicts ;
  S.propagate constraints ;
  constraints

(** low level call to the sat solver

    @param tested: optional int array used to cache older results
*)
let solve ~tested ~explain solver request =
  S.reset solver.constraints ;
  let result solve collect var =
    (* Real call to the SAT solver *)
    if solve solver.constraints var then
      if explain then (
        let l = S.assignment_true solver.constraints in
        if not (Option.is_none tested) then
          List.iter (fun i -> (Option.get tested).(i) <- true) l ;
        Diagnostic.SuccessInt (fun () -> l))
      else (
        (if not (Option.is_none tested) then
         let l = S.assignment_true solver.constraints in
         List.iter (fun i -> (Option.get tested).(i) <- true) l) ;
        Diagnostic.SuccessInt (fun () -> []))
    else if explain then
      Diagnostic.FailureInt (fun () -> collect solver.constraints var)
    else Diagnostic.FailureInt (fun () -> [])
  in
  match (request, solver.globalid) with
  | ([], ((false, false), _)) ->
      Diagnostic.SuccessInt (fun () -> [])
  | ([], (((_, true) | (true, _)), gid)) ->
      result S.solve S.collect_reasons (solver.map#vartoint gid)
  | ([i], ((false, false), _)) ->
      result S.solve S.collect_reasons (solver.map#vartoint i)
  | (l, ((false, false), _)) ->
      let il = List.map solver.map#vartoint l in
      result S.solve_lst S.collect_reasons_lst il
  | (l, (_, gid)) ->
      let il = List.map solver.map#vartoint (gid :: l) in
      result S.solve_lst S.collect_reasons_lst il

(* this function is used to "distcheck" a list of packages. The id is a cudfpool index *)
let pkgcheck callback solver tested id =
  let res =
    if not tested.(id) then solve ~tested:(Some tested) ~explain:false solver [id]
    else
      (* this branch is true only if the package was previously
         added to the tested packages and therefore it is installable
         if all = true then the solver is called again to provide the list
         of installed packages despite the fact the the package was already
         tested. This is done to provide one installation set for each package
         in the universe *)
      Diagnostic.SuccessInt (fun () -> [])
  in
  match res with
  | Diagnostic.SuccessInt _ ->
      callback (res, [id]) ;
      true
  | Diagnostic.FailureInt _ ->
      callback (res, [id]) ;
      false

(** low level constraint solver initialization

    @param univ cudf package universe
*)
let init_solver_univ univ =
  let map = new Util.identity in
  (* here we convert a cudfpool in a varpool. The assumption
   * that cudf package identifiers are contiguous is essential ! *)
  let (`CudfPool (keep_constraints, pool)) =
    init_pool_univ univ
  in
  let varpool = `SolverPool pool in
  let constraints = init_solver_cache ~explain:false varpool in
  let gid = Cudf.universe_size univ in
  { constraints; map; globalid = ((keep_constraints, false), gid) }

(* pool = cudf pool - closure = dependency clousure . cudf uid list *)

(** low level constraint solver initialization

    @param buffer debug buffer to print out debug messages
    @param pool dependencies and conflicts array idexed by package id
    @param closure subset of packages used to initialize the solver
*)
let init_solver_closure
    (`CudfPool (keep_constraints, cudfpool)) closure =
  let gid = Array.length cudfpool - 1 in
  let map = new Util.intprojection (List.length closure) in
  List.iter map#add closure ;
  let varpool =
    init_solver_pool map (`CudfPool (keep_constraints, cudfpool)) closure
  in
  let constraints = init_solver_cache varpool in
  { constraints; map; globalid = ((keep_constraints, false), gid) }

(***********************************************************)

let dependency_closure_cache (`CudfPool (_, cudfpool)) idlist =
  let queue = Queue.create () in
  let globalid = Array.length cudfpool - 1 in
  let visited = Hashtbl.create (2 * List.length idlist) in
  List.iter
    (fun e -> Queue.add (e, 0) queue)
    (CudfAdd.normalize_set (globalid :: idlist)) ;
  while Queue.length queue > 0 do
    let (id, level) = Queue.take queue in
    if (not (Hashtbl.mem visited id)) && level < max_int then (
      Hashtbl.add visited id () ;
      let (l, _) = cudfpool.(id) in
      List.iter
        (fun (_, dsj) ->
              List.iter
                (fun i ->
                  if not (Hashtbl.mem visited i) then
                    Queue.add (i, level + 1) queue)
                dsj)
        l)
  done ;
  Hashtbl.fold (fun k _ l -> k :: l) visited []

(*    XXX : elements in idlist should be included only if because
 *    of circular dependencies *)
