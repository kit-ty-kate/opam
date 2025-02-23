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
  val is_symlink : t -> bool
  val patch : ?preprocess:bool -> OpamFilename.t -> t -> exn option OpamProcess.job
  val make : t -> unit
  val dirs : t -> OpamFilename.Dir.t list
  val is_empty : t -> bool
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
end

val make_tar_gz_job : Tar.t -> Dir.t -> exn option OpamProcess.job
val extract_in_job : Tar.t -> Dir.t -> exn option OpamProcess.job
