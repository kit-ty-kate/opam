module Dir = struct
  type t = OpamFilename.Dir.t

  let of_dir = Fun.id
  let mk_tmp = OpamFilename.mk_tmp_dir
  let quarantine d =
    OpamFilename.Dir.of_string (OpamFilename.Dir.to_string d ^ ".new")
  let with_tmp = OpamFilename.with_tmp_dir

  let to_string = OpamFilename.Dir.to_string
  let to_dir = Fun.id

  let subdir d path = OpamFilename.Op.(d / path)
  let rmdir = OpamFilename.rmdir
  let dirname = OpamFilename.dirname_dir
  let exists = OpamFilename.exists_dir
  let subfile d file = OpamFilename.Op.(d // file)
  let remove_prefix = OpamFilename.remove_prefix_dir
  let mkdir = OpamFilename.mkdir
  let dirs = OpamFilename.dirs
  let is_empty = OpamFilename.dir_is_empty
  let basename = OpamFilename.basename_dir
  let copy = OpamFilename.copy_dir
  let is_symlink = OpamFilename.is_symlink_dir
  let move = OpamFilename.move_dir
  let cleandir = OpamFilename.cleandir
  let to_attribute = OpamFilename.to_attribute
  let in_dir = OpamFilename.in_dir
  let clone_name_in d d' = OpamFilename.Op.(d / OpamFilename.Base.to_string (basename d'))
end

module Tar = struct
  type t = OpamFilename.t

  let of_file = Fun.id

  let to_string = OpamFilename.to_string
  let to_file = Fun.id

  let exists = OpamFilename.exists
  let remove = OpamFilename.remove
  let clone_name_in d f = OpamFilename.create d (OpamFilename.basename f)
  let copy = OpamFilename.copy
end

let extract_in = OpamFilename.extract_in
let extract_in_job = OpamFilename.extract_in_job
let make_tar_gz_job = OpamFilename.make_tar_gz_job
