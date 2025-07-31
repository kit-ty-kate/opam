module Dir = struct
  type t = OpamFilename.Dir.t

  let of_dir = Fun.id
  let to_dir = Fun.id
  let to_string = OpamFilename.Dir.to_string

  let quarantine repo_root = OpamFilename.raw_dir (to_string repo_root ^ ".new")
  let with_tmp = OpamFilename.with_tmp_dir
  let backup ~tmp_dir repo_root =
    OpamFilename.Op.(tmp_dir / OpamFilename.Base.to_string (OpamFilename.basename_dir repo_root))

  let in_dir = OpamFilename.in_dir
  let exists = OpamFilename.exists_dir
  let remove = OpamFilename.rmdir
  let clean = OpamFilename.cleandir
  let move = OpamFilename.move_dir
  let copy = OpamFilename.copy_dir
  let is_symlink = OpamFilename.is_symlink_dir
  let patch = OpamFilename.patch
  let make = OpamFilename.mkdir
  let dirs = OpamFilename.dirs
  let is_empty = OpamFilename.dir_is_empty

  let repo repo_root = OpamFilename.Op.(repo_root // "repo" |> OpamFile.make)
end

module Tar = struct
  type t = OpamFilename.t

  let of_file = Fun.id
  let to_file = Fun.id
  let to_string = OpamFilename.to_string

  let backup ~tmp_dir tar =
    OpamFilename.create tmp_dir (OpamFilename.basename tar)

  let exists = OpamFilename.exists
  let remove = OpamFilename.remove
  let extract_in = OpamFilename.extract_in
  let download_as = OpamDownload.download_as
  let copy = OpamFilename.copy
  let move = OpamFilename.move
  let is_symlink = OpamFilename.is_symlink
end

let make_tar_gz_job = OpamFilename.make_tar_gz_job
let extract_in_job = OpamFilename.extract_in_job

type t =
  | Dir of Dir.t
  | Tar of Tar.t

let quarantine = function
  | Dir dir -> Dir (Dir.quarantine dir)
  | Tar tar -> Tar (OpamFilename.raw (Tar.to_string tar ^ ".new"))

let remove = function
  | Dir dir -> Dir.remove dir
  | Tar tar -> Tar.remove tar

let exists = function
  | Dir dir -> Dir.exists dir
  | Tar tar -> Tar.exists tar

let is_empty = function
  | Dir dir -> Dir.is_empty dir
  | Tar _tar -> false

let make = function
  | Dir dir -> Dir.make dir
  | Tar _tar -> () (* Creating an empty tar file doesn't make sense *)

let dirname = function
  | Dir dir -> OpamFilename.dirname_dir (Dir.to_dir dir)
  | Tar tar -> OpamFilename.dirname (Tar.to_file tar)

let basename = function
  | Dir dir -> OpamFilename.basename_dir (Dir.to_dir dir)
  | Tar tar -> OpamFilename.basename (Tar.to_file tar)

let to_string = function
  | Dir dir -> Dir.to_string dir
  | Tar tar -> Tar.to_string tar

let copy ~src ~dst =
  match src, dst with
  | Dir src, Dir dst -> Dir.copy ~src ~dst
  | Tar src, Tar dst -> Tar.copy ~src ~dst
  | Tar _, Dir _ -> assert false (* TODO *)
  | Dir _, Tar _ -> assert false (* TODO *)

let move ~src ~dst =
  match src, dst with
  | Dir src, Dir dst -> Dir.move ~src ~dst
  | Tar src, Tar dst -> Tar.move ~src ~dst
  | Tar _, Dir _ -> assert false (* TODO *)
  | Dir _, Tar _ -> assert false (* TODO *)

let is_symlink = function
  | Dir dir -> Dir.is_symlink dir
  | Tar tar -> Tar.is_symlink tar

let patch ?preprocess patch = function
  | Dir dir -> Dir.patch ?preprocess patch dir
  | Tar _ -> assert false (* TODO *)

let clean = function
  | Dir dir -> Dir.clean dir
  | Tar tar -> Tar.remove tar

let delayed_read_repo = function
  | Dir dir ->
    let repo_file_path = Dir.repo dir in
    let read () = OpamFile.Repo.safe_read repo_file_path in
    (OpamFile.exists repo_file_path, read)
  | Tar _ ->
    assert false (* TODO *)
