module Dir : sig
  type t

  val of_dir : OpamFilename.Dir.t -> t
  val to_dir : t -> OpamFilename.Dir.t
  val to_string : t -> string

  val quarantine : t -> t
  val with_tmp : (t -> 'a) -> 'a
  val backup : tmp_dir:OpamFilename.Dir.t -> t -> t

  val in_dir : t -> (unit -> 'a) -> 'a
  val exists : t -> bool
  val remove : t -> unit
  val clean : t -> unit
  val move : src:t -> dst:t -> unit
  val copy : src:t -> dst:t -> unit
  val copy_except_vcs : src:t -> dst:t -> unit
  val is_symlink : t -> bool
  val patch : ?preprocess:bool -> allow_unclean:bool -> OpamFilename.t -> t -> exn option
  val make : t -> unit
  val dirs : t -> OpamFilename.Dir.t list
  val is_empty : t -> bool option

  val repo : t -> OpamFile.Repo.t OpamFile.t
end

module Tar : sig
  type t

  val of_file : OpamFilename.t -> t
  val to_file : t -> OpamFilename.t
  val to_string : t -> string

  val backup : tmp_dir:OpamFilename.Dir.t -> t -> t

  val exists : t -> bool
  val remove : t -> unit
  val extract_in : t -> OpamFilename.Dir.t -> unit
  val download_as :
    ?quiet:bool ->
    ?validate:bool ->
    overwrite:bool ->
    ?compress:bool ->
    ?checksum:OpamHash.t ->
    OpamUrl.t -> t -> unit OpamProcess.job
  val copy : src:t -> dst:t -> unit
  val move : src:t -> dst:t -> unit
end

val make_tar_gz_job : Tar.t -> Dir.t -> exn option OpamProcess.job
val extract_in_job : Tar.t -> Dir.t -> exn option OpamProcess.job

type t =
  | Dir of Dir.t
  | Tar of Tar.t

val quarantine : t -> t
val remove : t -> unit
val exists : t -> bool
val is_empty : t -> bool option
val make : t -> unit
val dirname : t -> OpamFilename.Dir.t
val basename : t -> OpamFilename.Base.t
val to_string : t -> string
val copy : src:t -> dst:t -> unit
val move : src:t -> dst:t -> unit
val is_symlink : t -> bool
val patch : ?preprocess:bool -> allow_unclean:bool -> OpamFilename.t -> t -> exn option
val clean : t -> unit
val delayed_read_repo : t -> bool * (unit -> OpamFile.Repo.t)
