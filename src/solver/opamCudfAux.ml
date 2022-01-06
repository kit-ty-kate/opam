(** Encode - Decode *)

(* Specialized hashtable for encoding strings efficiently. *)
module EncodingHashtable = Hashtbl.Make (struct
  type t = string

  let equal = ( = )

  let hash s = Char.code s.[0]
end)

(* Specialized hashtable for decoding strings efficiently. *)
module DecodingHashtable = Hashtbl.Make (struct
  type t = string

  let equal = ( = )

  let hash s = (Char.code s.[1] * 1000) + Char.code s.[2]
end)

(* "hex_char char" returns the ASCII code of the given character
   in the hexadecimal form, prefixed with the '%' sign.
   e.g. hex_char '+' = "%2b" *)
(* let hex_char char = Printf.sprintf "%%%02x" (Char.code char);; *)

(* "init_hashtables" initializes the two given hashtables to contain:

    - enc_ht: Precomputed results of applying the function "hex_char"
    to all possible ASCII chars.
    e.g. EncodingHashtable.find enc_ht "+" = "%2b"

    - dec_ht: An inversion of enc_ht.
    e.g. DecodingHashtable.find dec_ht "%2b" = "+"
*)
let init_hashtables enc_ht dec_ht =
  let n = ref 255 in
  while !n >= 0 do
    let schr = String.make 1 (Char.chr !n) in
    let hchr = Printf.sprintf "%%%02x" !n in
    EncodingHashtable.add enc_ht schr hchr ;
    DecodingHashtable.add dec_ht hchr schr ;
    decr n
  done

(* Create and initialize twin hashtables,
   one for encoding and one for decoding. *)
let enc_ht = EncodingHashtable.create 256

let dec_ht = DecodingHashtable.create 256

let () = init_hashtables enc_ht dec_ht

let encode_single s = EncodingHashtable.find enc_ht s
let not_allowed_regexp = Re.Pcre.regexp "[^a-zA-Z0-9@/+().-]"
let encode s = Re.Pcre.substitute ~rex:not_allowed_regexp ~subst:encode_single s

let decode_single s = DecodingHashtable.find dec_ht s
let encoded_char_regexp = Re.Pcre.regexp "%[0-9a-f][0-9a-f]"
let decode s = Re.Pcre.substitute ~rex:encoded_char_regexp ~subst:decode_single s
