exception Unsat
exception Error of string

type reason =
  | Dependency of (Cudf.package * Cudf_types.vpkg list * Cudf.package list)
  | Missing of (Cudf.package * Cudf_types.vpkg list)
  | Conflict of (Cudf.package * Cudf.package * Cudf_types.vpkg)
type request = Cudf.package list
type result =
  | Success of (unit -> Cudf.package list)
  | Failure of (unit -> reason list)
type diagnosis = { result : result; request : request }
type solver_result_sat = (Cudf.preamble option * Cudf.universe)
type solver_result =
  | Sat of solver_result_sat
  | Unsat of diagnosis option
