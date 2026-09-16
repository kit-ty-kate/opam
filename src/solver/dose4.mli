open OpamSolverTypes

val dummy_request : Cudf.package
val check_request_using : call_solver:(Cudf.cudf -> solver_result_sat) option -> Cudf.cudf -> solver_result
val listcheck : callback:(diagnosis -> unit) -> Cudf.universe -> Cudf.package list -> int
val edos_install : Cudf.universe -> Cudf.package -> diagnosis
val edos_coinstall : Cudf.universe -> Cudf.package list -> diagnosis
