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

module Pcre = Re_pcre

let equal = Cudf.( =% )

let compare = Cudf.( <% )

let hash p = Hashtbl.hash (p.Cudf.package, p.Cudf.version)

module Cudf_hashtbl = Hashtbl.Make (struct
  type t = Cudf.package

  let equal = equal

  let hash = hash
end)

module Cudf_set = Set.Make (struct
  type t = Cudf.package

  let compare = compare
end)

(** Encode - Decode *)

(* Specialized hashtable for encoding strings efficiently. *)
module EncodingHashtable = Hashtbl.Make (struct
  type t = string

  let equal = ( = )

  let hash s = Char.code s.[0]
end)

(* Specialized hashtable for decoding strings efficiently. *)
module DecodingHashtable = Hashtbl.Make (struct
  type t = string

  let equal = ( = )

  let hash s = (Char.code s.[1] * 1000) + Char.code s.[2]
end)

(* "hex_char char" returns the ASCII code of the given character
   in the hexadecimal form, prefixed with the '%' sign.
   e.g. hex_char '+' = "%2b" *)
(* let hex_char char = Printf.sprintf "%%%02x" (Char.code char);; *)

(* "init_hashtables" initializes the two given hashtables to contain:

    - enc_ht: Precomputed results of applying the function "hex_char"
    to all possible ASCII chars.
    e.g. EncodingHashtable.find enc_ht "+" = "%2b"

    - dec_ht: An inversion of enc_ht.
    e.g. DecodingHashtable.find dec_ht "%2b" = "+"
*)
let init_hashtables enc_ht dec_ht =
  let n = ref 255 in
  while !n >= 0 do
    let schr = String.make 1 (Char.chr !n) in
    let hchr = Printf.sprintf "%%%02x" !n in
    EncodingHashtable.add enc_ht schr hchr ;
    DecodingHashtable.add dec_ht hchr schr ;
    decr n
  done

(* Create and initialize twin hashtables,
   one for encoding and one for decoding. *)
let enc_ht = EncodingHashtable.create 256

let dec_ht = DecodingHashtable.create 256;;

init_hashtables enc_ht dec_ht

(* encode *)
let encode_single s = EncodingHashtable.find enc_ht s

let not_allowed_regexp = Pcre.regexp "[^a-zA-Z0-9@/+().-]"

let encode s = Pcre.substitute ~rex:not_allowed_regexp ~subst:encode_single s

(* decode *)
let decode_single s = DecodingHashtable.find dec_ht s

let encoded_char_regexp = Pcre.regexp "%[0-9a-f][0-9a-f]"

let decode s = Pcre.substitute ~rex:encoded_char_regexp ~subst:decode_single s

(** Pretty Printing *)

let string_of pp arg =
  ignore (pp Format.str_formatter arg) ;
  Format.flush_str_formatter ()

let pp_version fmt pkg =
  try
    Format.fprintf fmt "%s" (decode (Cudf.lookup_package_property pkg "number"))
  with Not_found -> Format.fprintf fmt "%d" pkg.Cudf.version

let pp_package fmt pkg =
  Format.fprintf fmt "%s (= %a)" (decode pkg.Cudf.package) pp_version pkg

let string_of_package = string_of pp_package

type pp =
  Cudf.package ->
  string * string option * string * (string * (string * bool)) list

module StringSet = Set.Make (String)

let add_to_package_list h n p =
  try
    let l = Hashtbl.find h n in
    l := p :: !l
  with Not_found -> Hashtbl.add h n (ref [p])

let add_properties preamble l =
  List.fold_left
    (fun pre prop -> { pre with Cudf.property = prop :: pre.Cudf.property })
    preamble
    l

let pkgtoint = Cudf.uid_by_package

let inttopkg = Cudf.package_by_uid

let normalize_set (l : int list) =
  List.rev
    (List.fold_left
       (fun results x -> if List.mem x results then results else x :: results)
       []
       l)

(* vpkg -> pkg list *)
let who_provides univ (pkgname, constr) =
  let pkgl = Cudf.lookup_packages ~filter:constr univ pkgname in
  let prol = Cudf.who_provides ~installed:false univ (pkgname, constr) in
  let filter = function
    | (p, None) -> Some p
    | (p, Some v) when Cudf.version_matches v constr -> Some p
    | _ -> None
  in
  pkgl @ List.filter_map filter prol

(* vpkg -> id list *)
let resolve_vpkg_int univ vpkg =
  List.map (Cudf.uid_by_package univ) (who_provides univ vpkg)

(* vpkg list -> id list *)
let resolve_vpkgs_int univ vpkgs =
  normalize_set (List.flatten (List.map (resolve_vpkg_int univ) vpkgs))

(* vpkg list -> pkg list *)
let resolve_deps univ vpkgs =
  List.map (Cudf.package_by_uid univ) (resolve_vpkgs_int univ vpkgs)

(* pkg -> pkg list list *)
let who_depends univ pkg = List.map (resolve_deps univ) pkg.Cudf.depends

type ctable = (int, int list ref) Hashtbl.t
