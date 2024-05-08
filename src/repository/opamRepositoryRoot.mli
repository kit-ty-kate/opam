type t

val of_name : OpamFilename.Dir.t -> OpamRepositoryName.t -> t

val repo_name : t -> OpamRepositoryName.t

val does_not_exist_or_is_empty : t -> bool

val parent_dir : t -> OpamFilename.Dir.t
val basename : t -> OpamFilename.Base.t

val unsafe_dirname : t -> OpamFilename.Dir.t

(** tmp_repo *)

type tmp_repo

val get_tmp_repo : t -> tmp_repo

val fetch_from_local_dir : OpamFilename.Dir.t -> tmp_repo -> unit
val copy_previous_state : t -> tmp_repo -> unit
val copy_new_state : tmp_repo -> t -> unit

val unsafe_tmp_repo_to_dir : tmp_repo -> OpamFilename.Dir.t
val unsafe_tmp_repo_basename : tmp_repo -> OpamFilename.Base.t

val finalise : tmp_repo -> unit
