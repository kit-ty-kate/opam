(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes

let log = OpamConsole.log "REPO_BACKEND"
let slog = OpamConsole.slog

type update =
  | Update_full of dirname
  | Update_patch of (filename * Patch.t list)
  | Update_empty
  | Update_err of exn

module type S = sig
  val name: OpamUrl.backend
  val pull_url:
    ?cache_dir:dirname -> ?subpath:subpath -> dirname -> OpamHash.t option -> url ->
    filename option download OpamProcess.job
  val fetch_repo_update:
    repository_name -> ?cache_dir:dirname -> dirname -> url ->
    update OpamProcess.job
  val repo_update_complete: dirname -> url -> unit OpamProcess.job
  val revision: dirname -> version option OpamProcess.job
  val sync_dirty:
    ?subpath:subpath -> dirname -> url -> filename option download OpamProcess.job
  val get_remote_url:
    ?hash:string -> dirname ->
    url option OpamProcess.job
end

let compare r1 r2 = compare r1.repo_name r2.repo_name

let to_string r =
  Printf.sprintf "%s from %s"
    (OpamRepositoryName.to_string r.repo_name)
    (OpamUrl.to_string r.repo_url)

let to_json r =
  `O  [ ("name", OpamRepositoryName.to_json r.repo_name);
        ("kind", `String (OpamUrl.string_of_backend r.repo_url.OpamUrl.backend));
      ]

let check_digest filename = function
  | Some expected
    when OpamRepositoryConfig.(!r.force_checksums) <> Some false ->
    (match OpamHash.mismatch (OpamFilename.to_string filename) expected with
     | None -> true
     | Some bad_hash ->
       OpamConsole.error
         "Bad checksum for %s: expected %s\n\
         \                     got      %s\n\
          Metadata might be out of date, in this case use `opam update`."
         (OpamFilename.to_string filename)
         (OpamHash.to_string expected)
         (OpamHash.to_string bad_hash);
       false)
  | _ -> true

let job_text name label =
  OpamProcess.Job.with_text
    (Printf.sprintf "[%s: %s]"
       (OpamConsole.colorise `green (OpamRepositoryName.to_string name))
       label)

let get_diff parent_dir dir1 dir2 =
  log "diff: %a/{%a,%a}"
    (slog OpamFilename.Dir.to_string) parent_dir
    (slog OpamFilename.Base.to_string) dir1
    (slog OpamFilename.Base.to_string) dir2;
  let open (struct
    type state =
      | Mine
      | Their
      | Both
  end) in
  let module DiffMap = Map.Make (String) in
  let rec aux dir1 dir2 =
    let files =
      List.fold_left (fun map file -> DiffMap.add file Mine map)
        DiffMap.empty (OpamStd.Option.map_default OpamSystem.get_files [] dir1)
    in
    let files =
      List.fold_left (fun map file ->
          DiffMap.update file (function
              | None -> Some Their
              | Some _ -> Some Both) map)
        files (OpamStd.Option.map_default OpamSystem.get_files [] dir2)
    in
    let diffs =
      DiffMap.fold (fun file state diffs ->
          let file1, file2 = match state with
            | Mine -> Some (Filename.concat (Option.get dir1) file), None
            | Their -> None, Some (Filename.concat (Option.get dir2) file)
            | Both ->
              Some (Filename.concat (Option.get dir1) file),
              Some (Filename.concat (Option.get dir2) file)
          in
          let add_to_diffs content1 content2 diffs =
            match Patch.diff ~filename:file content1 content2 with
            | None -> diffs
            | Some diff -> diff :: diffs
          in
          match OpamStd.Option.map Unix.lstat file1, OpamStd.Option.map Unix.lstat file2 with
          | Some {Unix.st_kind = Unix.S_REG; _}, None
          | None, Some {Unix.st_kind = Unix.S_REG; _}
          | Some {Unix.st_kind = Unix.S_REG; _}, Some {Unix.st_kind = Unix.S_REG; _} ->
            let content1 = Option.map OpamSystem.read file1 in
            let content2 = Option.map OpamSystem.read file2 in
            add_to_diffs content1 content2 diffs
          | Some {Unix.st_kind = Unix.S_DIR; _}, None
          | None, Some {Unix.st_kind = Unix.S_DIR; _}
          | Some {Unix.st_kind = Unix.S_DIR; _}, Some {Unix.st_kind = Unix.S_DIR; _} ->
            aux file1 file2
          | Some {Unix.st_kind = Unix.S_LNK; _}, None
          | None, Some {Unix.st_kind = Unix.S_LNK; _}
          | Some {Unix.st_kind = Unix.S_LNK; _}, Some {Unix.st_kind = Unix.S_LNK; _} ->
            assert false (* TODO *)
          | Some {Unix.st_kind = Unix.S_REG; _}, Some {Unix.st_kind = Unix.S_DIR; _} ->
            assert false (* TODO *)
          | Some {Unix.st_kind = Unix.S_DIR; _}, Some {Unix.st_kind = Unix.S_REG; _} ->
            assert false (* TODO *)
          | Some {Unix.st_kind = Unix.S_REG; _}, Some {Unix.st_kind = Unix.S_LNK; _} ->
            assert false (* TODO *)
          | Some {Unix.st_kind = Unix.S_LNK; _}, Some {Unix.st_kind = Unix.S_REG; _} ->
            assert false (* TODO *)
          | Some {Unix.st_kind = Unix.S_LNK; _}, Some {Unix.st_kind = Unix.S_DIR; _} ->
            assert false (* TODO *)
          | Some {Unix.st_kind = Unix.S_DIR; _}, Some {Unix.st_kind = Unix.S_LNK; _} ->
            assert false (* TODO *)
          | Some {Unix.st_kind = Unix.S_CHR; _}, _ | _, Some {Unix.st_kind = Unix.S_CHR; _} ->
            failwith (Printf.sprintf "Character devices (%s) are unsupported" file)
          | Some {Unix.st_kind = Unix.S_BLK; _}, _ | _, Some {Unix.st_kind = Unix.S_BLK; _} ->
            failwith (Printf.sprintf "Block devices (%s) are unsupported" file)
          | Some {Unix.st_kind = Unix.S_FIFO; _}, _ | _, Some {Unix.st_kind = Unix.S_FIFO; _} ->
            failwith (Printf.sprintf "Named pipes (%s) are unsupported" file)
          | Some {Unix.st_kind = Unix.S_SOCK; _}, _ | _, Some {Unix.st_kind = Unix.S_SOCK; _} ->
            failwith (Printf.sprintf "Sockets (%s) are unsupported" file)
          | None, None -> assert false)
        files []
    in
    diffs
  in
  match aux (Some (OpamFilename.Base.to_string dir1)) (Some (OpamFilename.Base.to_string dir2)) with
  | [] -> None
  | diffs ->
    let patch = OpamSystem.temp_file ~auto_clean: false "patch" in
    let patch_file = OpamFilename.of_string patch in
    OpamFilename.write patch_file (Format.asprintf "%a" (Patch.pp_list ~git:true) diffs);
    Some (patch_file, diffs)
