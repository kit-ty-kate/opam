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
end

let make_tar_gz_job = OpamFilename.make_tar_gz_job
let extract_in_job = OpamFilename.extract_in_job
