(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

module Pcre = Re_pcre

let check_fail file =
  let ic = open_in file in
  try
    let l = input_line ic in
    try
      close_in ic ;
      l = "FAIL"
    with Scanf.Scan_failure _ ->
      close_in ic ;
      false
  with End_of_file ->
    close_in ic ;
    false

let prng = lazy (Random.State.make_self_init ())

(* bits and pieces borrowed from ocaml stdlib/filename.ml *)
let mktmpdir prefix suffix =
  let temp_dir = try Sys.getenv "TMPDIR" with Not_found -> "/tmp" in
  let temp_file_name temp_dir prefix suffix =
    let rnd = Random.State.bits (Lazy.force prng) land 0xFFFFFF in
    Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)
  in
  let rec try_name counter =
    let name = temp_file_name temp_dir prefix suffix in
    try
      Unix.mkdir name 0o700 ;
      name
    with Unix.Unix_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in
  try_name 0

let rmtmpdir path =
  (try
     Sys.remove (Filename.concat path "in-cudf") ;
     Sys.remove (Filename.concat path "out-cudf")
   with _e -> ()) ;
  Unix.rmdir path

let rec input_all_lines acc chan =
  try input_all_lines (input_line chan :: acc) chan with End_of_file -> acc

(** Solver "exec:" line. Contains three named wildcards to be interpolated:
   "$in", "$out", and "$pref"; corresponding to, respectively, input CUDF
   document, output CUDF universe, user preferences. *)

(* remove all characters disallowed in criteria *)
(* TODO: should this really be done? *)
let sanitize s =
  Pcre.substitute
    ~rex:(Pcre.regexp "[^\\[\\]+()a-z0-9,\"-]")
    ~subst:(fun _ -> "")
    s

exception Error of string

exception Unsat

let raise_error fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

let check_exit_status cmd = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED i -> raise_error "command '%s' failed with code %d" cmd i
  | Unix.WSIGNALED i -> raise_error "command '%s' killed by signal %d" cmd i
  | Unix.WSTOPPED i -> raise_error "command '%s' stopped by signal %d" cmd i

let try_set_close_on_exec fd =
  try
    Unix.set_close_on_exec fd ;
    true
  with Invalid_argument _ -> false

let open_proc_full cmd env input output error toclose =
  let cloexec = List.for_all try_set_close_on_exec toclose in
  match Unix.fork () with
  | 0 -> (
      Unix.dup2 input Unix.stdin ;
      Unix.close input ;
      Unix.dup2 output Unix.stdout ;
      Unix.close output ;
      Unix.dup2 error Unix.stderr ;
      Unix.close error ;
      if not cloexec then List.iter Unix.close toclose ;
      try Unix.execvpe (List.hd cmd) (Array.of_list cmd) env
      with _ -> exit 127)
  | id -> id

(* bits and pieces borrowed from ocaml stdlib/filename.ml *)
let open_process argv env =
  let (in_read, in_write) = Unix.pipe () in
  let fds_to_close = ref [in_read; in_write] in
  try
    let (out_read, out_write) = Unix.pipe () in
    fds_to_close := out_read :: out_write :: !fds_to_close ;
    let (err_read, err_write) = Unix.pipe () in
    fds_to_close := err_read :: err_write :: !fds_to_close ;
    let inchan = Unix.in_channel_of_descr in_read in
    let outchan = Unix.out_channel_of_descr out_write in
    let errchan = Unix.in_channel_of_descr err_read in
    let pid =
      open_proc_full
        argv
        env
        out_read
        in_write
        err_write
        [in_read; out_write; err_read]
    in
    Unix.close out_read ;
    Unix.close in_write ;
    Unix.close err_write ;
    (inchan, outchan, errchan, pid)
  with e ->
    List.iter Unix.close !fds_to_close ;
    raise e

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let close_process (inchan, outchan, errchan, pid) =
  close_in inchan ;
  (try close_out outchan with Sys_error _ -> ()) ;
  close_in errchan ;
  snd (waitpid_non_intr pid)
