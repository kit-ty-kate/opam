val dummy_request : Cudf.package

val check_request_using :
  call_solver:(Cudf.cudf -> OpamSolverTypes.solver_result_sat) option ->
  Cudf.cudf ->
  OpamSolverTypes.solver_result
