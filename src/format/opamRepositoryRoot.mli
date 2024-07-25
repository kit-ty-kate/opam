module Dir : sig
  type t

  val of_dir : OpamFilename.Dir.t -> t
  val mk_tmp : unit -> t
  val quarantine : t -> t
  val with_tmp : (t -> 'a) -> 'a

  val to_string : t -> string
  val to_dir : t -> OpamFilename.Dir.t

  val subdir : t -> string -> OpamFilename.Dir.t
  val rmdir : t -> unit
  val dirname : t -> OpamFilename.Dir.t
  val exists : t -> bool
  val subfile : t -> string -> OpamFilename.t
  val remove_prefix : t -> OpamFilename.Dir.t -> string
  val mkdir : t -> unit
  val dirs : t -> OpamFilename.Dir.t list
  val is_empty : t -> bool
  val basename : t -> OpamFilename.Base.t
  val copy : src:t -> dst:t -> unit
  val is_symlink : t -> bool
  val move : src:t -> dst:t -> unit
  val cleandir : t -> unit
  val to_attribute : t -> OpamFilename.t -> OpamFilename.Attribute.t
  val in_dir : t -> (unit -> 'a) -> 'a
  val clone_name_in : OpamFilename.Dir.t -> t -> t
end

module Tar : sig
  type t

  val of_file : OpamFilename.t -> t

  val to_string : t -> string
  val to_file : t -> OpamFilename.t

  val exists : t -> bool
  val remove : t -> unit
  val clone_name_in : OpamFilename.Dir.t -> t -> t
  val copy : src:t -> dst:t -> unit
end

val extract_in : Tar.t -> OpamFilename.Dir.t -> unit
val extract_in_job : Tar.t -> Dir.t -> exn option OpamProcess.job
val make_tar_gz_job : Tar.t -> Dir.t -> exn option OpamProcess.job
