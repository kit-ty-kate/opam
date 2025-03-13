open Tar.Syntax

let rec safe_read fd buf off len =
  try Unix.read fd buf off len
  with Unix.Unix_error (Unix.EINTR, _, _) -> safe_read fd buf off len

let rec run : type a. Unix.file_descr -> (a, _, _) Tar.t -> a = fun fd -> function
  | Tar.Read len ->
      let b = Bytes.create len in
      let read = safe_read fd b 0 len in
      if read = 0 then
        failwith "unexpected end of file"
      else if len = (read : int) then
        Bytes.unsafe_to_string b
      else
        Bytes.sub_string b 0 read
  | Tar.Really_read len ->
      let rec loop fd buf offset len =
        if offset < (len : int) then
          let n = safe_read fd buf offset (len - offset) in
          if n = 0 then
            failwith "unexpected end of file"
          else
            loop fd buf (offset + n) len
      in
      let buf = Bytes.create len in
      loop fd buf 0 len;
      Bytes.unsafe_to_string buf
  | Tar.Return (Ok x) -> x
  | Tar.Return (Error _) -> failwith "something's gone wrong"
  | Tar.High _ | Tar.Write _ | Tar.Seek _ -> assert false
  | Tar.Bind (x, f) -> run fd (f (run fd x))

type content =
  | File of string
  | Dir
  | Hardlink of string
  | Symlink of string

let fold fd f x =
  let go ?global:_ hdr x =
    match hdr.Tar.Header.link_indicator with
    | Normal ->
      let* content = Tar.really_read (Int64.to_int hdr.file_size) in
      let x = f x hdr.file_name (File content) in
      Tar.return (Ok x)
    | Directory ->
      if hdr.Tar.Header.file_size <> 0L then
        failwith "something went wrong";
      let x = f x hdr.file_name Dir in
      Tar.return (Ok x)
    | Hard ->
      if hdr.Tar.Header.file_size <> 0L then
        failwith "something went wrong";
      let x = f x hdr.file_name (Hardlink hdr.link_name) in
      Tar.return (Ok x)
    | Symbolic ->
      if hdr.Tar.Header.file_size <> 0L then
        failwith "something went wrong";
      let x = f x hdr.file_name (Symlink hdr.link_name) in
      Tar.return (Ok x)
    | Character -> failwith "char devices unsupported"
    | Block -> failwith "block devices unsupported"
    | FIFO -> failwith "fifo unsupported"
    | GlobalExtendedHeader -> failwith "global extended header unsupported"
    | PerFileExtendedHeader -> failwith "perfile extended header unsupported"
    | LongLink -> failwith "longlinks unsupported"
    | LongName -> failwith "longnames unsupported"
  in
  run fd (Tar_gz.in_gzipped (Tar.fold go x))
