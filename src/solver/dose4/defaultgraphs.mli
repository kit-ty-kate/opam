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
