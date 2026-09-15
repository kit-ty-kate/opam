exception Unsat
exception Error of string

module Defaultgraphs : sig
  module GraphOper (G : Graph.Sig.I) : sig
    module O : Graph.Oper.S with type g = G.t
  end
  module PackageGraph : sig
    module G : Graph.Sig.I
      with type V.t = Cudf.package
       and type V.label = Cudf.package
       and type E.t = Cudf.package * Cudf.package
       and type E.label = unit
    module UG : Graph.Sig.I
      with type V.t = Cudf.package
       and type V.label = Cudf.package
       and type E.t = Cudf.package * Cudf.package
       and type E.label = unit
    module DotPrinter : sig
      val output_graph : out_channel -> G.t -> unit
    end
    val conflict_graph : Cudf.universe -> UG.t
  end
end

module Diagnostic : sig
  type reason =
    | Dependency of (Cudf.package * Cudf_types.vpkg list * Cudf.package list)
    | Missing of (Cudf.package * Cudf_types.vpkg list)
    | Conflict of (Cudf.package * Cudf.package * Cudf_types.vpkg)
  type request = Cudf.package list
  type result =
    | Success of (unit -> Cudf.package list)
    | Failure of (unit -> reason list)
 type diagnosis = { result : result; request : request }
 val is_solution : diagnosis -> bool
end

module CudfAdd : sig
  val compare : Cudf.package -> Cudf.package -> int
  val equal : Cudf.package -> Cudf.package -> bool
  val hash : Cudf.package -> int
  val encode : string -> string
  val decode : string -> string
  val add_properties : Cudf.preamble -> Cudf_types.typedecl -> Cudf.preamble
  val resolve_deps : Cudf.universe -> Cudf_types.vpkglist -> Cudf.package list
  val who_depends : Cudf.universe -> Cudf.package -> Cudf.package list list
end

module Depsolver : sig
  val dummy_request : Cudf.package
  type solver_result =
    | Sat of (Cudf.preamble option * Cudf.universe)
    | Unsat of Diagnostic.diagnosis option
  val check_request_using :
    call_solver:(Cudf.cudf -> Cudf.preamble option * Cudf.universe) ->
    Cudf.cudf ->
    solver_result
  val check_request : Cudf.cudf -> solver_result
  val listcheck :
    callback:(Diagnostic.diagnosis -> unit) ->
    Cudf.universe ->
    Cudf.package list ->
    int
  val edos_install : Cudf.universe -> Cudf.package -> Diagnostic.diagnosis
  val edos_coinstall : Cudf.universe -> Cudf.package list -> Diagnostic.diagnosis
end
