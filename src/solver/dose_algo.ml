open Dose_common

module Defaultgraphs = struct
(** generic operation over imperative graphs *)
(* this is a VERY expensive operation on Labelled graphs ... *)
module GraphOper (G : Graph.Sig.I) = struct
  module O = Graph.Oper.I (G)
end

(******************************************************)

(* Note: ConcreteBidirectionalLabelled graphs are slower and we do not use them
   here *)

(** Imperative bidirectional graph for dependecies.
    Imperative unidirectional graph for conflicts. *)
module PackageGraph = struct
  module PkgV = struct
    type t = Cudf.package

    let compare = CudfAdd.compare

    let hash = CudfAdd.hash

    let equal = CudfAdd.equal
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectional (PkgV)
  module UG = Graph.Imperative.Graph.Concrete (PkgV)

  module DotPrinter = struct
    module Display = struct
      include G

      let vertex_name v = Printf.sprintf "\"%s\"" (CudfAdd.string_of_package v)

      let graph_attributes _ = []

      let get_subgraph _ = None

      let default_edge_attributes _ = []

      let default_vertex_attributes _ = []

      let vertex_attributes p =
        if p.Cudf.installed then [`Color 0x00FF00] else []

      let edge_attributes _ = []
    end

    include Graph.Graphviz.Dot (Display)
  end

  let conflict_graph_aux gr universe pkg =
    List.iter
      (fun (pkgname, constr) ->
        List.iter
          (UG.add_edge gr pkg)
          (CudfAdd.who_provides universe (pkgname, constr)))
      pkg.Cudf.conflicts

  (** Build the conflict graph from the given cudf universe *)
  let conflict_graph universe =
    let gr = UG.create () in
    Cudf.iter_packages (conflict_graph_aux gr universe) universe ;
    gr
end
end

module Diagnostic = struct
type reason_int =
  | DependencyInt of (int * Cudf_types.vpkg list * int list)
  | MissingInt of (int * Cudf_types.vpkg list)
  | ConflictInt of (int * int * Cudf_types.vpkg)

type result_int =
  | SuccessInt of (unit -> int list)
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
  | Success of (unit -> Cudf.package list)
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
        (fun () ->
          List.filter_map
            (function
              | i when i = globalid -> None
              | i ->
                  Some
                    { (from_sat (map#inttovar i)) with Cudf.installed = true })
            (f_int ()))
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

let get_installationset = function
  | { result = Success f; _ } -> f ()
  | { result = Failure _; _ } -> raise Not_found

let is_solution = function
  | { result = Success _; _ } -> true
  | { result = Failure _; _ } -> false
end

module Depsolver_int = struct
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
  callback (res, [id]) ;
  match res with
  | Diagnostic.SuccessInt _ -> true
  | Diagnostic.FailureInt _ -> false

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
  { constraints; map = (map :> Dose_common.Util.projection); globalid = ((keep_constraints, false), gid) }

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
end

module Depsolver = struct
type solver = Depsolver_int.solver

(** [listcheck ?callback universe pkglist] check if a subset of packages
    un the universe are installable.

    @param pkglist list of packages to be checked
    @return the number of packages that cannot be installed
*)
let listcheck ~callback universe pkglist =
  let aux ~callback univ idlist =
    let solver = Depsolver_int.init_solver_univ univ in
    let failed = ref 0 in
    let size = Cudf.universe_size univ + 1 in
    let tested = Array.make size false in
    let check = Depsolver_int.pkgcheck callback solver tested in
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
  let callback_int (res, req) =
    callback (Diagnostic.diagnosis map universe res req)
  in
  aux ~callback:callback_int universe idlist

let edos_install_cache univ cudfpool pkglist =
  let idlist = List.map (CudfAdd.pkgtoint univ) pkglist in
  let closure = Depsolver_int.dependency_closure_cache cudfpool idlist in
  let solver =
    Depsolver_int.init_solver_closure cudfpool closure
  in
  let res = Depsolver_int.solve solver ~tested:None ~explain:true idlist in
  Diagnostic.diagnosis solver.Depsolver_int.map univ res idlist

let edos_install universe pkg =
  let cudfpool = Depsolver_int.init_pool_univ universe in
  edos_install_cache universe cudfpool [pkg]

let edos_coinstall universe pkglist =
  let cudfpool = Depsolver_int.init_pool_univ universe in
  edos_install_cache universe cudfpool pkglist

type solver_result =
  | Sat of (Cudf.preamble option * Cudf.universe)
  | Unsat of Diagnostic.diagnosis option

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

let remove_dummy pre (dummy, d) =
  if Diagnostic.is_solution d then
    let is =
      Util.list_remove_if (Cudf.( =% ) dummy) (Diagnostic.get_installationset d)
    in
    Sat (Some pre, Cudf.load_universe is)
  else
    Unsat (Some d)

let check_request_using ~call_solver (pre, universe, request) =
  match call_solver with
  | None ->
      let (u, r) = add_dummy universe request dummy_request in
      remove_dummy pre (r, edos_install u r)
  | Some call_solver -> (
      try
        Sat (call_solver (pre, universe, request))
      with
      | CudfSolver.Unsat ->
          let (u, r) = add_dummy universe request dummy_request in
          remove_dummy pre (r, edos_install u r))

(** check if a cudf request is satisfiable. we do not care about
    universe consistency . We try to install a dummy package *)
let check_request cudf =
  check_request_using ~call_solver:None cudf

let check_request_using ~call_solver cudf =
  check_request_using ~call_solver:(Some call_solver) cudf

type depclean_result =
  Cudf.package
  * (Cudf_types.vpkglist * Cudf_types.vpkg * Cudf.package list) list
  * (Cudf_types.vpkg * Cudf.package list) list
end
