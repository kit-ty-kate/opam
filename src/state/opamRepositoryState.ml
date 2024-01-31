(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStd.Op
open OpamStateTypes

let log fmt = OpamConsole.log "RSTATE" fmt
let slog = OpamConsole.slog

module Cache = struct
  type t = {
    cached_repofiles: (repository_name * OpamFile.Repo.t) list;
    cached_opams: (repository_name * OpamFile.OPAM.t OpamPackage.Map.t) list;
  }

  module C = OpamCached.Make (struct
      type nonrec t = t
      let name = "repository"
    end)

  let remove () =
    let root = OpamStateConfig.(!r.root_dir) in
    let cache_dir = OpamPath.state_cache_dir root in
    let remove_cache_file file =
      if OpamFilename.check_suffix file ".cache" then
        OpamFilename.remove file
    in
    List.iter remove_cache_file (OpamFilename.files cache_dir)

  let marshall rt =
    (* Repository without remote are not cached, they are intended to be
       manually edited *)
    let filter_out_nourl repos_map =
      OpamRepositoryName.Map.filter
        (fun name _ ->
           try
             (OpamRepositoryName.Map.find name rt.repositories).repo_url <>
             OpamUrl.empty
           with Not_found -> false)
        repos_map
    in
      { cached_repofiles =
          OpamRepositoryName.Map.bindings
            (filter_out_nourl rt.repos_definitions);
        cached_opams =
          OpamRepositoryName.Map.bindings
            (filter_out_nourl rt.repo_opams);
      }

  let file rt =
    OpamPath.state_cache rt.repos_global.root

  let save rt =
    remove ();
    C.save (file rt) (marshall rt)

  let save_new rt =
    C.save (file rt) (marshall rt)

  let load root =
    let file = OpamPath.state_cache root in
    match C.load file with
    | Some cache ->
      Some
        (OpamRepositoryName.Map.of_list cache.cached_repofiles,
         OpamRepositoryName.Map.of_list cache.cached_opams)
    | None -> None

end

let load_opams_from_dir repo_name repo_root =
  (* FIXME: why is this different from OpamPackage.list ? *)
  let rec aux r dir =
    if OpamFilename.exists_dir dir then
      let fnames = Sys.readdir (OpamFilename.Dir.to_string dir) in
      if Array.exists (fun f -> f = "opam") fnames then
        match OpamFileTools.read_repo_opam ~repo_name ~repo_root dir with
        | Some opam ->
          (try
             let nv =
               OpamPackage.of_string
                 OpamFilename.(Base.to_string (basename_dir dir))
             in
             OpamPackage.Map.add nv opam r
           with Failure _ ->
             log "ERR: directory name not a valid package: ignored %s"
               OpamFilename.(to_string Op.(dir // "opam"));
             r)
        | None ->
          log "ERR: Could not load %s, ignored"
            OpamFilename.(to_string Op.(dir // "opam"));
          r
      else
        Array.fold_left (fun r name -> aux r OpamFilename.Op.(dir / name))
          r fnames
    else r
  in
  aux OpamPackage.Map.empty (OpamRepositoryPath.packages_dir repo_root)

let load_repo_from_dir repo repo_root =
  let repo_def =
    OpamFile.Repo.safe_read (OpamRepositoryPath.repo repo_root)
    |> OpamFile.Repo.with_root_url repo.repo_url
  in
  let opams = load_opams_from_dir repo.repo_name repo_root in
  (repo_def, opams)

module Tar_gz = Tar_gz.Make
  (struct type 'a t = 'a
          let ( >>= ) x f = f x
          let return x = x end)
  (struct type out_channel = Stdlib.out_channel
          type 'a io = 'a
          let really_write = Stdlib.output_string end)
  (struct type in_channel = Stdlib.in_channel
          type 'a io = 'a
          let read ic buf = Stdlib.input ic buf 0 (Bytes.length buf) end)

let get_content_from_tar_gz ic hdr =
  let buf = Bytes.create (Int64.to_int hdr.Tar.Header.file_size) in
  Tar_gz.really_read ic buf;
  Bytes.unsafe_to_string buf

let load_repo_from_tar_gz repo repo_tar_gz =
  let ic = Stdlib.open_in_bin (OpamFilename.to_string repo_tar_gz) in
  let ic = Tar_gz.of_in_channel ~internal:(De.bigstring_create De.io_buffer_size) ic in
  let rec loop global repo_def opams =
    match Tar_gz.HeaderReader.read ~global ic with
    | Ok (hdr, global) ->
        let data_length, repo_def, opams =
          let filename = hdr.Tar.Header.file_name in
          if String.equal filename "repo" then begin
            let repo_def =
              get_content_from_tar_gz ic hdr
              |> OpamFile.Repo.read_from_string ~filename:(OpamFile.make (OpamFilename.of_string filename)) (* false filename but it'll be fiiinnne *)
              |> OpamFile.Repo.with_root_url repo.repo_url
            in
            (0 (* Data already read *), Some repo_def, opams)
          end else if OpamStd.String.ends_with ~suffix:"/opam" filename then begin
            let opam =
              get_content_from_tar_gz ic hdr
              |> OpamFile.OPAM.read_from_string ~filename:(OpamFile.make (OpamFilename.of_string filename)) (* false filename but it'll be fiiinnne *)
            in
            let pkg =
              let list = String.split_on_char '/' filename |> List.rev in
              OpamPackage.of_string (List.nth list 1)
            in
            let opams = OpamPackage.Map.add pkg opam opams in
            (0 (* Data already read *), repo_def, opams)
          end else begin
            (Int64.to_int hdr.Tar.Header.file_size, repo_def, opams)
          end
        in
        let data_padding = Tar.Header.compute_zero_padding_length hdr in
        Tar_gz.skip ic (data_length + data_padding);
        loop global repo_def opams
    | Error (`Fatal _) -> failwith "malformed tar.gz file"
    | Error `Eof -> (repo_def, opams)
  in
  let repo_def, opams = loop None None OpamPackage.Map.empty in
  match repo_def with
  | None -> failwith "repo file not found"
  | Some repo_def -> (repo_def, opams)

let load_repo repo repo_root =
  let t = OpamConsole.timer () in
  let repo_def, opams =
    match repo_root with
    | OpamFilename.D repo_root ->
      load_repo_from_dir repo repo_root
    | OpamFilename.F repo_tar_gz ->
      load_repo_from_tar_gz repo repo_tar_gz
  in
  log "loaded opam files from repo %s in %.3fs"
    (OpamRepositoryName.to_string repo.repo_name)
    (t ());
  repo_def, opams

(* Cleaning directories follows the repo path pattern:
   TMPDIR/opam-tmp-dir/repo-dir, defined in [load]. *)
let clean_repo_tmp tmp_dir =
  if Lazy.is_val tmp_dir then
    (let dir = Lazy.force tmp_dir in
     OpamFilename.rmdir dir;
     let parent = OpamFilename.dirname_dir dir in
     if OpamFilename.dir_is_empty parent then
       OpamFilename.rmdir parent)

let remove_from_repos_tmp rt name =
  try
    clean_repo_tmp (Hashtbl.find rt.repos_tmp name);
    Hashtbl.remove rt.repos_tmp name
  with Not_found -> ()

let cleanup rt =
  Hashtbl.iter (fun _ tmp_dir -> clean_repo_tmp tmp_dir) rt.repos_tmp;
  Hashtbl.clear rt.repos_tmp

let get_root_raw root repos_tmp name =
  match Hashtbl.find repos_tmp name with
  | lazy repo_root -> repo_root
  | exception Not_found -> OpamRepositoryPath.root root name

let get_root_location repos_locations name =
  match OpamRepositoryName.Map.find_opt name repos_locations with
  | Some repo_location -> repo_location
  | None -> assert false (* TODO *)

let get_root rt name =
  get_root_raw rt.repos_global.root rt.repos_tmp name

let get_repo_root rt repo =
  get_root_raw rt.repos_global.root rt.repos_tmp repo.repo_name

let load lock_kind gt =
  OpamFormatUpgrade.as_necessary_repo_switch_light_upgrade lock_kind `Repo gt;
  log "LOAD-REPOSITORY-STATE %@ %a" (slog OpamFilename.Dir.to_string) gt.root;
  let lock = OpamFilename.flock lock_kind (OpamPath.repos_lock gt.root) in
  let repos_map = OpamStateConfig.Repos.safe_read ~lock_kind gt in
  if OpamStateConfig.is_newer_than_self gt then
    log "root version (%s) is greater than running binary's (%s); \
         load with best-effort (read-only)"
      (OpamVersion.to_string (OpamFile.Config.opam_root_version gt.config))
      (OpamVersion.to_string (OpamFile.Config.root_version));
  let mk_repo name url_opt = {
    repo_name = name;
    repo_url = OpamStd.Option.Op.((url_opt >>| fst) +! OpamUrl.empty);
    repo_trust = OpamStd.Option.Op.(url_opt >>= snd);
  } in
  let uncached =
    (* Don't cache repositories without remote, as they should be editable
       in-place *)
    OpamRepositoryName.Map.filter (fun _ url -> url = None) repos_map
  in
  let repositories = OpamRepositoryName.Map.mapi mk_repo repos_map in
  let repos_tmp_root = lazy (OpamFilename.mk_tmp_dir ()) in
  let repos_tmp = Hashtbl.create 23 in
  OpamRepositoryName.Map.iter (fun name repo ->
      let uncompressed_root = OpamRepositoryPath.root gt.root repo.repo_name in
      let tar = OpamRepositoryPath.tar gt.root repo.repo_name in
      if not (OpamFilename.exists_dir uncompressed_root) &&
         OpamFilename.exists tar
      then
        let tmp = lazy (
          let tmp_root = Lazy.force repos_tmp_root in
          try
            (* We rely on this path pattern to clean the repo.
               cf. [clean_repo_tmp] *)
            OpamFilename.extract_in tar tmp_root;
            OpamFilename.Op.(tmp_root / OpamRepositoryName.to_string name)
          with Failure s ->
            OpamFilename.remove tar;
            OpamConsole.error_and_exit `Aborted
              "%s.\nRun `opam update --repositories %s` to fix the issue"
              s (OpamRepositoryName.to_string name);
        ) in
        Hashtbl.add repos_tmp name tmp
    ) repositories;
  let repos_locations =
    OpamRepositoryName.Map.mapi (fun name _ ->
        let tar_gz = OpamRepositoryPath.tar gt.root name in
        if Sys.file_exists (OpamFilename.to_string tar_gz) then
          OpamFilename.F tar_gz
        else
          OpamFilename.D (OpamRepositoryPath.root gt.root name)
    ) repos_map
  in
  let make_rt repos_definitions opams =
    let rt = {
      repos_global = (gt :> unlocked global_state);
      repos_lock = lock;
      repos_tmp;
      repositories;
      repos_definitions;
      repo_opams = opams;
      repos_locations;
    } in
    OpamStd.Sys.at_exit (fun () -> cleanup rt);
    rt
  in
  match Cache.load gt.root with
  | Some (repofiles, opams) when OpamRepositoryName.Map.is_empty uncached ->
    log "Cache found";
    make_rt repofiles opams
  | Some (repofiles, opams) ->
    log "Cache found, loading repositories without remote only";
    OpamFilename.with_flock_upgrade `Lock_read lock @@ fun _ ->
    let repofiles, opams =
      OpamRepositoryName.Map.fold (fun name url (defs, opams) ->
          let repo = mk_repo name url in
          let repo_def, repo_opams =
            load_repo repo (get_root_location repos_locations name)
          in
          OpamRepositoryName.Map.add name repo_def defs,
          OpamRepositoryName.Map.add name repo_opams opams)
        uncached (repofiles, opams)
    in
    make_rt repofiles opams
  | None ->
    log "No cache found";
    OpamFilename.with_flock_upgrade `Lock_read lock @@ fun _ ->
    let repofiles, opams =
      OpamRepositoryName.Map.fold (fun name url (defs, opams) ->
          let repo = mk_repo name url in
          let repo_def, repo_opams =
            load_repo repo (get_root_location repos_locations name)
          in
          OpamRepositoryName.Map.add name repo_def defs,
          OpamRepositoryName.Map.add name repo_opams opams)
        repos_map (OpamRepositoryName.Map.empty, OpamRepositoryName.Map.empty)
    in
    let rt = make_rt repofiles opams in
    Cache.save_new rt;
    rt

let find_package_opt rt repo_list nv =
  List.fold_left (function
      | None ->
        fun repo_name ->
          OpamStd.Option.Op.(
            OpamRepositoryName.Map.find_opt repo_name rt.repo_opams >>=
            OpamPackage.Map.find_opt nv >>| fun opam ->
            repo_name, opam
          )
      | some -> fun _ -> some)
    None repo_list

let build_index rt repo_list =
  List.fold_left (fun acc repo_name ->
      try
        let repo_opams = OpamRepositoryName.Map.find repo_name rt.repo_opams in
        OpamPackage.Map.union (fun a _ -> a) acc repo_opams
      with Not_found ->
        (* A repo is unavailable, error should have been already reported *)
        acc)
    OpamPackage.Map.empty
    repo_list

let get_repo rt name = OpamRepositoryName.Map.find name rt.repositories

let unlock ?cleanup:(cln=true) rt =
  if cln then cleanup rt;
  OpamSystem.funlock rt.repos_lock;
  (rt :> unlocked repos_state)

let drop ?cleanup rt =
  let _ = unlock ?cleanup rt in ()

let with_write_lock ?dontblock rt f =
  if OpamStateConfig.is_newer_than_self rt.repos_global then
    OpamConsole.error_and_exit `Locked
      "The opam root has been upgraded by a newer version of opam-state \
       and cannot be written to";
  let ret, rt =
    OpamFilename.with_flock_upgrade `Lock_write ?dontblock rt.repos_lock
    @@ fun _ -> f ({ rt with repos_lock = rt.repos_lock } : rw repos_state)
    (* We don't actually change the field value, but this makes restricting the
       phantom lock type possible *)
  in
  ret, { rt with repos_lock = rt.repos_lock }

let with_ lock gt f =
  let rt = load lock gt in
  OpamStd.Exn.finally (fun () -> drop rt) (fun () -> f rt)

let write_config rt =
  OpamFile.Repos_config.write (OpamPath.repos_config rt.repos_global.root)
    (OpamRepositoryName.Map.map (fun r ->
         if r.repo_url = OpamUrl.empty then None
         else Some (r.repo_url, r.repo_trust))
        rt.repositories)

let check_last_update () =
  if OpamCoreConfig.(!r.debug_level) < 0 then () else
  let last_update =
    OpamFilename.written_since
      (OpamPath.state_cache (OpamStateConfig.(!r.root_dir)))
  in
  if last_update > float_of_int (3600*24*21) then
    OpamConsole.note "It seems you have not updated your repositories \
                      for a while. Consider updating them with:\n%s\n"
      (OpamConsole.colorise `bold "opam update");
