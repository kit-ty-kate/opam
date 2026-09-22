(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 Kate Deplaix                                         *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamCudfSolverSig

let log ?level f = OpamConsole.log "0install" ?level f

let name = "builtin-0install"

let ext = ref None

let is_present () = true

let command_name = None

let preemptive_check = false

let default_criteria = {
  crit_default = "-changed,\
                  -count[avoid-version,solution]";
  crit_upgrade = "-count[avoid-version,solution]";
  crit_fixup = "-count[avoid-version,solution]";
  crit_best_effort_prefix = None;
}

let not_relop = function
  | `Eq -> `Neq
  | `Neq -> `Eq
  | `Geq -> `Lt
  | `Gt -> `Leq
  | `Leq -> `Gt
  | `Lt -> `Geq

let keep_installed ~drop_installed_packages request pkgname =
  not drop_installed_packages &&
  not (List.exists (fun (pkg, _) -> String.equal pkg pkgname) request.Cudf.install) &&
  not (List.exists (fun (pkg, _) -> String.equal pkg pkgname) request.Cudf.upgrade) &&
  not (List.exists (fun (pkg, _) -> String.equal pkg pkgname) request.Cudf.remove)

let add_spec pkg req c (pkgs, constraints) =
  let pkgs = (pkg, req) :: pkgs in
  let constraints = match c with
    | None -> constraints
    | Some c -> (pkg, c) :: constraints
  in
  (pkgs, constraints)

let essential spec (pkg, c) = add_spec pkg `Essential c spec
let recommended spec (pkg, c) = add_spec pkg `Recommended c spec

let restricts (pkgs, constraints) (pkg, c) =
  let constraints = match c with
    | None -> (pkg, (`Lt, 1)) :: (pkg, (`Gt, 1)) :: constraints (* pkg < 1 & pkg > 1 is always false *)
    | Some (relop, v) -> (pkg, (not_relop relop, v)) :: constraints
  in
  (pkgs, constraints)

let create_spec ~drop_installed_packages universe request =
  let spec = ([], []) in
  let spec = List.fold_left essential spec request.Cudf.install in
  let spec = List.fold_left essential spec request.Cudf.upgrade in
  let spec = List.fold_left restricts spec request.Cudf.remove in
  Cudf.fold_packages_by_name (fun spec pkgname pkgs ->
      match List.find_opt (fun pkg -> pkg.Cudf.installed) pkgs with
      | Some {Cudf.keep = `Keep_version; version; _} -> essential spec (pkgname, Some (`Eq, version))
      | Some {Cudf.keep = `Keep_package; _} -> essential spec (pkgname, None)
      | Some {Cudf.keep = `Keep_feature; _} -> assert false (* NOTE: Opam has no support for features *)
      | Some {Cudf.keep = `Keep_none; _} ->
          if keep_installed ~drop_installed_packages request pkgname then
            recommended spec (pkgname, None)
          else
            spec
      | None -> spec
    ) spec universe

let reconstruct_universe universe selections =
  Opam_0install_cudf.packages_of_result selections |>
  List.fold_left (fun pkgs (pkg, v) ->
      let pkg = Cudf.lookup_package universe (pkg, v) in
      {pkg with was_installed = pkg.installed; installed = true} :: pkgs
    ) [] |>
  Cudf.load_universe

type options = {
  drop_installed_packages : bool;
  prefer_oldest : bool;
  handle_avoid_version : bool;
  prefer_installed : bool;
}

let parse_criteria criteria =
  let default =
    {
      drop_installed_packages = false;
      prefer_oldest = false;
      handle_avoid_version = false;
      prefer_installed = false;
    }
  in
  let rec parse default (criteria : OpamCudfCriteria.criterion list) =
    match criteria with
    | [] -> default
    | (Plus, Removed, None)::xs ->
      parse {default with drop_installed_packages = true} xs
    | (Plus, Solution, Some "version-lag")::xs ->
      parse {default with prefer_oldest = true} xs
    | (Minus, Solution, Some "avoid-version")::xs ->
      parse {default with handle_avoid_version = true} xs
    | (Minus, Changed, None)::xs ->
      parse {default with prefer_installed = true} xs
    | criterion::xs ->
      OpamConsole.warning
        "Criteria '%s' is not supported by the 0install solver"
        (OpamCudfCriteria.criterion_to_string criterion);
      parse default xs
  in
  parse default (OpamCudfCriteria.of_string criteria)

let call ~criteria ?timeout:_ ?tolerance:_ (preamble, universe, request) =
  let {
    drop_installed_packages;
    prefer_oldest;
    handle_avoid_version;
    prefer_installed;
  } =
    parse_criteria criteria
  in
  let timer = OpamConsole.timer () in
  let pkgs, constraints = create_spec ~drop_installed_packages universe request in
  let context =
    Opam_0install_cudf.create
      ~prefer_oldest ~handle_avoid_version ~prefer_installed
      ~constraints universe
  in
  match Opam_0install_cudf.solve context pkgs with
  | Ok selections ->
    let universe = reconstruct_universe universe selections in
    log "Solution found. Solve took %.2f s" (timer ());
    OpamSolverTypes.Sat (Some preamble, universe)
  | Error problem ->
    log "No solution. Solve took %.2f s" (timer ());
    log ~level:3 "%a" (OpamConsole.slog Opam_0install_cudf.diagnostics) problem;
    OpamSolverTypes.Unsat (Some (fun () ->
        let module Diag = Opam_0install_cudf.Raw_diagnostics in
        List.iter (fun {Diag.role; outcome; notes} ->
            let rec pp_role = function
              | Diag.Real pkgname -> pkgname
              | Diag.Virtual impls -> String.concat "|" (List.map pp_impl impls)
            and pp_impl = function
              | Diag.RealImpl {pkg; requires = _} -> Printf.sprintf "%s.%d" pkg.Cudf.package pkg.Cudf.version
              | Diag.VirtualImpl deps -> String.concat "&" (List.map pp_dependency deps)
              | Diag.Reject (pkgname, version) -> Printf.sprintf "%s.%d" pkgname version
              | Diag.Dummy -> "(no version)"
            and pp_dependency {Diag.drole; importance; restrictions} =
              Printf.sprintf "(%s %s %s)" (pp_role drole) (pp_importance importance) (String.concat " & " (List.map pp_restriction restrictions))
            and pp_importance = function
              | `Essential -> "(essential)"
              | `Recommended -> "(recommended)"
              | `Restricts -> "(restricts)"
            and pp_restriction {Diag.kind; expr} =
              Printf.sprintf "(%s %s)" (pp_kind kind) (String.concat " & " (List.map pp_constr expr))
            and pp_kind = function
              | `Ensure -> "ensure"
              | `Prevent -> "prevent"
            and pp_constr (relop, version) =
              Printf.sprintf "%s %d" (pp_relop relop) version
            and pp_relop = function
              | `Lt -> "<"
              | `Gt -> ">"
              | `Leq -> "<="
              | `Geq -> ">="
              | `Neq -> "!="
              | `Eq -> "="
            and pp_outcome = function
              | Diag.SelectedImpl impl -> pp_impl impl
              | Diag.RejectedCandidates (rejects, candidate_kind) ->
                Printf.sprintf "%s\n  - %s" (pp_candidate_kind candidate_kind) (String.concat "\n  - " (List.map pp_reject rejects))
            and pp_candidate_kind = function
              | `All_unusable -> "all unusable"
              | `No_candidates -> "no candidates"
              | `Conflicts -> "conflicts"
            and pp_reject (impl, reason) =
              Printf.sprintf "%s: %s" (pp_impl impl) (pp_reason reason)
            and pp_reason = function
              | ModelRejection vpkg -> Printf.sprintf "ModelRejection %s" (pp_vpkg vpkg)
              | FailsRestriction restriction -> Printf.sprintf "FailsRestriction %s" (pp_restriction restriction)
              | DepFailsRestriction (dependency, restriction) -> Printf.sprintf "DepFailsRestriction (%s, %s)" (pp_dependency dependency) (pp_restriction restriction)
              | ConflictsRole role -> Printf.sprintf "ConflictsRole %s" (pp_role role)
              | DiagnosticsFailure msg -> Printf.sprintf "DiagnosticsFailure %s" msg
            and pp_vpkg (pkgname, constr) =
              match constr with
              | None -> pkgname
              | Some constr -> Printf.sprintf "%s %s" pkgname (pp_constr constr)
            and pp_note = function
              | Diag.UserRequested restriction -> Printf.sprintf "UserRequested %s" (pp_restriction restriction)
              | Diag.ReplacesConflict role -> Printf.sprintf "ReplacesConflict %s" (pp_role role)
              | Diag.ReplacedByConflict role -> Printf.sprintf "ReplacedByConflict %s" (pp_role role)
              | Diag.Restricts (role, impl, restrictions) -> Printf.sprintf "Restricts (%s, %s, %s)" (pp_role role) (pp_impl impl) (String.concat " & " (List.map pp_restriction restrictions))
              | Diag.Feed_problem msg -> Printf.sprintf "Feed_problem %s" msg
            in
            Printf.printf "%s -> %s\n  - (%s)\n" (pp_role role) (pp_outcome outcome) (String.concat ", " (List.map pp_note notes))
          ) (Diag.get problem);
        []
      ))
