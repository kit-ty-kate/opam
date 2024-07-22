type t =
  | Directory of OpamFilename.Dir.t * OpamRepositoryName.t
  | TarGz of OpamFilename.t * OpamRepositoryName.t * OpamFilename.Dir.t

let of_name d name =
  Directory (d, name)

let with_tmp_root ~tmp_root f name =
  TarGz (f, name, tmp_root)

let from_tmp_dir _ =
  assert false

let repo_name = function
  | Directory (_, name)
  | TarGz (_, name, _) -> name

let parent_dir = function
  | Directory (d, _) -> OpamFilename.dirname_dir d
  | TarGz (f, _, _) -> OpamFilename.dirname f

let basename = function
  | Directory (d, _) -> OpamFilename.basename_dir d
  | TarGz (f, _, _) -> OpamFilename.basename f

let unsafe_dirname _ =
  assert false

let cleanup _ =
  assert false

let exists = function
  | Directory (d, _) -> OpamFilename.exists_dir d
  | TarGz (f, _, _) -> OpamFilename.exists f

let does_not_exist_or_is_empty root =
  not (exists root) ||
  match root with
  | Directory (d, _) -> OpamFilename.dir_is_empty d
  | TarGz _ -> false

let move src_root dst_root =
  match src_root, dst_root with
  | Directory (src, _), Directory (dst, _) ->
    OpamFilename.move_dir ~src ~dst;
  | TarGz (src, _, _), TarGz (dst, _, _) ->
    OpamFilename.move ~src ~dst;
  | TarGz _, Directory _ ->
    assert false (* TODO *)
  | Directory _, TarGz _ ->
    assert false (* TODO *)

(** tmp_repo *)

type tmp_repo

let get_tmp_repo _ =
  assert false

let fetch_from_local_dir _ _ =
  assert false

let copy_previous_state _ _ =
  assert false

let copy_new_state _ _ =
  assert false

let move_to_new_state _ =
  assert false

let unsafe_tmp_repo_to_dir _ =
  assert false

let unsafe_tmp_repo_basename _ =
  assert false

let finalise _ =
  assert false

(** tar.gz *)

type tarred_repo

let get_tarred_repo _ =
  assert false

let make _ =
  assert false

let tarred_repo_exists _ =
  assert false

let tarred_repo_remove _ =
  assert false
