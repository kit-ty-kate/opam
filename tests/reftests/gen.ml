let first_line_tags ~path =
  let ic = open_in path in
  let s = input_line ic in
  close_in ic;
  String.split_on_char ' ' s

let dirname path =
  List.tl (List.rev path)

let null_hash = "N0REP0"
let prefix_hash = "PREFIX"
let default_repo = "opam-repo-"^null_hash

let diff_rule base_name ~condition =
  let path = String.concat "/" base_name in
  let base_name = String.concat "-" base_name in
  Format.sprintf
    {|
(rule
 (alias reftest-%s)%s
 (action
  (diff %s.test %s.out)))

(alias
 (name reftest)%s
 (deps (alias reftest-%s)))
|}
    base_name condition path path condition base_name

let tgz_name ~archive_hash =
  Printf.sprintf "opam-archive-%s.tar.gz" archive_hash

let repo_directory ~archive_hash =
  Printf.sprintf "opam-repo-%s" archive_hash

let opamroot_directory ~archive_hash =
  Printf.sprintf "root-%s" archive_hash

let rec get_prefix_files dir_name =
  match dir_name with
  | [] -> ""
  | _::_ ->
    let prefix_file = dir_name @ ["prefix.test"] in
    if Sys.file_exists (String.concat Filename.dir_sep prefix_file) then
      let parent_prefix_files = get_prefix_files (dirname dir_name) in
      String.concat "/" prefix_file ^
      if parent_prefix_files = "" then
        ""
      else
        " " ^ parent_prefix_files
    else
      get_prefix_files (dirname dir_name)

let run_rule ~base_name ~deps ~condition =
  let base_name = String.concat "/" base_name in
  Format.sprintf {|
(rule
 (targets %s)
 (deps %s)%s
 (package opam)
 (action
  (with-stdout-to
   %%{targets}
   (run ./run.exe %%{exe:../../src/client/opamMain.exe.exe} %%{dep:%s.test} %%{read-lines:testing-env}))))
|} (base_name^".out") deps condition base_name

let archive_download_rule archive_hash =
   Format.sprintf {|
(rule
 (targets %s)
 (action (run curl --silent -Lo %%{targets} https://github.com/ocaml/opam-repository/archive/%s.tar.gz)))
|} (tgz_name ~archive_hash) archive_hash

let default_repo_rule =
  Format.sprintf {|
(rule
 (targets %s)
 (action
  (progn
   (run mkdir -p %%{targets}/packages)
   (write-file repo "opam-version:\"2.0\"")
   (run cp repo %%{targets}/repo))))
|} default_repo

(* XXX this fails if the directory already exists ?! *)
let archive_unpack_rule archive_hash =
  Format.sprintf {|
(rule
  (targets %s)
  (action
   (progn
    (run mkdir -p %%{targets})
    (run tar -C %%{targets} -xzf %%{dep:%s} --strip-components=1))))
|} (repo_directory ~archive_hash) (tgz_name ~archive_hash)

let opam_init_rule archive_hash =
  Format.sprintf {|
(rule
  (targets %s)
  (deps opam-root-version)
  (action
   (progn
    (ignore-stdout
    (run %%{bin:opam} init --root=%%{targets}
           --no-setup --bypass-checks --no-opamrc --bare
           file://%%{dep:%s})))))
|} (opamroot_directory ~archive_hash) (repo_directory ~archive_hash)

module StringSet = Set.Make(String)

let () =
  let () = set_binary_mode_out stdout true in
  let get_contents dir =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (function "." | ".." -> false | _ -> true)
    |> List.sort String.compare
  in
  let rec process archive_hashes filename =
    let base_name, is_level1_dir = match List.rev filename with
      | [] -> assert false
      | [basename; _] -> (basename, true)
      | basename::_ -> (basename, false)
    in
    let filename' = String.concat Filename.dir_sep filename in
    if OpamStd.String.ends_with ~suffix:".d" base_name &&
       Sys.is_directory filename' then
      let contents =
        List.map (fun x -> filename @ [x]) (get_contents filename')
      in
      List.fold_left process archive_hashes contents
    else if base_name = "prefix.test" then
      if filename = ["prefix.test"] then
        failwith "prefix.test is not accepted at the toplevel"
      else
        match first_line_tags ~path:filename', is_level1_dir with
        | [archive_hash], true ->
          if archive_hash = (null_hash : string) then
            archive_hashes
          else
            StringSet.add archive_hash archive_hashes
        | [archive_hash], false when archive_hash = (prefix_hash : string) ->
          archive_hashes
        | tags, _ ->
          failwith
            (Printf.sprintf "Tags '%s' are not allowed in prefix file %s"
               (String.concat " " tags) filename')
    else
      match OpamStd.String.rcut_at base_name '.' with
      | Some (base_name, "test") ->
        let archive_hash, tags =
          match first_line_tags ~path:filename' with
          | [] -> assert false (* String.split_on_char never returns [] *)
          | ar::tags -> ar, tags
        in
        let os_condition =
          match Filename.extension base_name with
          | "" -> ""
          | os ->
            Printf.sprintf "(= %%{os_type} %S)"
              String.(capitalize_ascii (sub os 1 (length os - 1)))
        in
        let dir_name = dirname filename in
        let base_name = dir_name @ [base_name] in
        let deps =
          if archive_hash = (prefix_hash : string) then
            get_prefix_files dir_name
          else
            opamroot_directory ~archive_hash
        in
        let tags_conditions =
          List.map (fun tag ->
              if tag = (null_hash : string) then
                Printf.sprintf "(= %%{env:TEST%s=0} 1)" tag
              else
                failwith
                  (Printf.sprintf "Tag '%s' unrecognized in test '%s'"
                     tag filename'))
            (if String.equal archive_hash null_hash
             then archive_hash :: tags else tags)
        in
        let condition =
          Printf.sprintf
            "\n (enabled_if (and %s (or (<> %%{env:TESTALL=1} 0) %s)))"
            os_condition (String.concat "\n   " tags_conditions)
        in
        print_string (diff_rule base_name ~condition);
        print_string (run_rule ~base_name ~deps ~condition);
        if archive_hash = (null_hash : string) ||
           archive_hash = (prefix_hash : string) then
          archive_hashes
        else
          StringSet.add archive_hash archive_hashes
      | None | Some _ -> archive_hashes
  in
  let archive_hashes =
    List.fold_left process
      StringSet.empty (List.map (fun x -> [x]) (get_contents "."))
  in
  print_string default_repo_rule;
  print_string (opam_init_rule null_hash);
  StringSet.iter
    (fun archive_hash ->
       print_string (archive_download_rule archive_hash);
       print_string (archive_unpack_rule archive_hash);
       print_string (opam_init_rule archive_hash)
    )
    archive_hashes
