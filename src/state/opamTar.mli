type content =
  | File of string
  | Dir
  | Hardlink of string
  | Symlink of string

val fold : Unix.file_descr -> ('a -> string -> content -> 'a) -> 'a -> 'a
