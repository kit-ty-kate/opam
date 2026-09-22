(**************************************************************************)
(*                                                                        *)
(*    Copyright 2026      Kate Deplaix                                    *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

type reason =
  | Dependency of (Cudf.package * Cudf_types.vpkg list * Cudf.package list)
  | Missing of (Cudf.package * Cudf_types.vpkg list)
  | Conflict of (Cudf.package * Cudf.package * Cudf_types.vpkg)

type sat_result =
  | Sat of (Cudf.preamble option * Cudf.universe)
  | Unsat of (unit -> reason list) option
