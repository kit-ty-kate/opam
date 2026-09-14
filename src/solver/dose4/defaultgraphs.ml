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
