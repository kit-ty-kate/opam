type t

let of_name _ _ =
  assert false

let with_tmp_root _ _ _ =
  assert false

let repo_name _ =
  assert false

let does_not_exist_or_is_empty _ =
  assert false

let parent_dir _ =
  assert false

let basename _ =
  assert false

let unsafe_dirname _ =
  assert false

let cleanup _ =
  assert false

let exists _ =
  assert false

let move _ _ =
  assert false

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
