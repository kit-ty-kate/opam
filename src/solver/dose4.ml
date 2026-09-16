exception Error of string
exception Unsat

module CudfAdd = struct
let normalize_set (l : int list) =
  List.rev
    (List.fold_left
       (fun results x -> if List.mem x results then results else x :: results)
       []
       l)

(* vpkg -> pkg list *)
let who_provides univ (pkgname, constr) =
  let pkgl = Cudf.lookup_packages ~filter:constr univ pkgname in
  let prol = Cudf.who_provides ~installed:false univ (pkgname, constr) in
  let filter = function
    | (p, None) -> Some p
    | (p, Some v) when Cudf.version_matches v constr -> Some p
    | _ -> None
  in
  pkgl @ List.filter_map filter prol

(* vpkg -> id list *)
let resolve_vpkg_int univ vpkg =
  List.map (Cudf.uid_by_package univ) (who_provides univ vpkg)

(* vpkg list -> id list *)
let resolve_vpkgs_int univ vpkgs =
  normalize_set (List.flatten (List.map (resolve_vpkg_int univ) vpkgs))

(* vpkg list -> pkg list *)
let resolve_deps univ vpkgs =
  List.map (Cudf.package_by_uid univ) (resolve_vpkgs_int univ vpkgs)

(* pkg -> pkg list list *)
let who_depends univ pkg = List.map (resolve_deps univ) pkg.Cudf.depends
end

module EdosSolver = struct
module type S = sig
  type reason
end

module IntHash = Hashtbl.Make (struct
  type t = int

  let equal = ( = )

  let hash i = i
end)

let ( @ ) l1 l2 =
  let rec geq = function
    | ([], []) -> true
    | (_ :: _, []) -> true
    | ([], _ :: _) -> false
    | (_ :: r1, _ :: r2) -> geq (r1, r2)
  in
  if geq (l1, l2) then List.append l2 l1 else List.append l1 l2

module M (X : S) = struct
  module X = X

  let debug = ref false

  (* Variables *)
  type var = int

  (* Literals *)
  type lit = int

  (* A clause is an array of literals *)
  type clause =
    { lits : lit array; all_lits : lit array; reasons : X.reason list }

  type value = True | False | Unknown

  module LitMap = Map.Make (struct
    type t = int

    let compare (x : int) y = compare x y
  end)

  type state =
    { (* Indexed by var *)
      st_assign : value array;
      st_assign_true : unit IntHash.t;
      st_reason : clause option array;
      st_level : int array;
      st_seen_var : int array;
      st_refs : int array;
      st_pinned : bool array;
      (* Indexed by lit *)
      st_simpl_prop : clause LitMap.t array;
      st_watched : clause list array;
      st_associated_vars : var list array;
      (* Queues *)
      mutable st_trail : lit list;
      mutable st_trail_lim : lit list list;
      st_prop_queue : lit Queue.t;
      (* Misc *)
      mutable st_cur_level : int;
      mutable st_min_level : int;
      mutable st_seen : int;
      mutable st_var_queue_head : var list;
      st_var_queue : var Queue.t;
      mutable st_cost : int;
      (* Total computational cost so far *)
      st_print_var : Format.formatter -> int -> unit;
    }

  (****)

  let charge st x = st.st_cost <- st.st_cost + x

  (* let get_bill st = st.st_cost *)

  (****)

  let pin_var st x = st.st_pinned.(x) <- true

  let unpin_var st x = st.st_pinned.(x) <- false

  let enqueue_var st x =
    charge st 1 ;
    pin_var st x ;
    Queue.push x st.st_var_queue

  (*
  let requeue_var st x =
    pin_var st x;
    st.st_var_queue_head <- x :: st.st_var_queue_head
*)

  (* Returns -1 if no variable remains *)
  let rec dequeue_var st =
    let x =
      match st.st_var_queue_head with
      | x :: r ->
          st.st_var_queue_head <- r ;
          x
      | [] -> ( try Queue.take st.st_var_queue with Queue.Empty -> -1)
    in
    if x = -1 then x
    else (
      unpin_var st x ;
      if st.st_refs.(x) = 0 || st.st_assign.(x) <> Unknown then dequeue_var st
      else x)

  (****)

  let var_of_lit p = p lsr 1

  let pol_of_lit p = p land 1 = 0

  let lit_of_var v s = if s then v + v else v + v + 1

  let lit_neg p = p lxor 1

  let val_neg v =
    match v with True -> False | False -> True | Unknown -> Unknown

  let val_of_bool b = if b then True else False

  let val_of_lit st p =
    let v = st.st_assign.(var_of_lit p) in
    if pol_of_lit p then v else val_neg v

  (****)

  let print_val ch v =
    Format.fprintf
      ch
      "%s"
      (match v with True -> "True" | False -> "False" | Unknown -> "Unknown")

  let print_lits st ch lits =
    Format.fprintf ch "{" ;
    Array.iter
      (fun p ->
        if pol_of_lit p then
          Format.fprintf ch " +%a" st.st_print_var (var_of_lit p)
        else Format.fprintf ch " -%a" st.st_print_var (var_of_lit p))
      lits ;
    Format.fprintf ch " }"

  let print_rule st ch r = print_lits st ch r.lits

  exception Conflict of clause option

  let enqueue st p reason =
    charge st 1 ;
    (if !debug then
     match reason with
     | Some r -> Format.eprintf "Applying rule %a@." (print_rule st) r
     | _ -> ()) ;
    match val_of_lit st p with
    | False ->
        if !debug then
          if pol_of_lit p then
            Format.eprintf "Cannot install %a@." st.st_print_var (var_of_lit p)
          else
            Format.eprintf
              "Already installed %a@."
              st.st_print_var
              (var_of_lit p) ;
        raise (Conflict reason)
    | True -> ()
    | Unknown ->
        if !debug then
          if pol_of_lit p then
            Format.eprintf "Installing %a@." st.st_print_var (var_of_lit p)
          else
            Format.eprintf
              "Should not install %a@."
              st.st_print_var
              (var_of_lit p) ;
        let x = var_of_lit p in
        st.st_assign.(x) <- val_of_bool (pol_of_lit p) ;
        if st.st_assign.(x) = True then IntHash.add st.st_assign_true x () ;
        st.st_reason.(x) <- reason ;
        st.st_level.(x) <- st.st_cur_level ;
        st.st_trail <- p :: st.st_trail ;
        List.iter
          (fun x ->
            charge st 1 ;
            let refs = st.st_refs.(x) in
            if refs = 0 then enqueue_var st x ;
            st.st_refs.(x) <- st.st_refs.(x) + 1)
          st.st_associated_vars.(p) ;
        Queue.push p st.st_prop_queue

  let rec find_not_false st lits i l =
    if i = l then -1
    else if val_of_lit st lits.(i) <> False then i
    else find_not_false st lits (i + 1) l

  let propagate_in_clause st r p =
    charge st 1 ;
    let p' = lit_neg p in
    if r.lits.(0) = p' then (
      r.lits.(0) <- r.lits.(1) ;
      r.lits.(1) <- p') ;
    if val_of_lit st r.lits.(0) = True then
      st.st_watched.(p) <- r :: st.st_watched.(p)
    else
      let i = find_not_false st r.lits 2 (Array.length r.lits) in
      if i = -1 then (
        st.st_watched.(p) <- r :: st.st_watched.(p) ;
        enqueue st r.lits.(0) (Some r))
      else (
        r.lits.(1) <- r.lits.(i) ;
        r.lits.(i) <- p' ;
        let p = lit_neg r.lits.(1) in
        st.st_watched.(p) <- r :: st.st_watched.(p))

  let propagate st =
    try
      while not (Queue.is_empty st.st_prop_queue) do
        charge st 1 ;
        let p = Queue.take st.st_prop_queue in
        LitMap.iter (fun p r -> enqueue st p (Some r)) st.st_simpl_prop.(p) ;
        let l = ref st.st_watched.(p) in
        st.st_watched.(p) <- [] ;
        try
          while
            match !l with
            | r :: rem ->
                l := rem ;
                propagate_in_clause st r p ;
                true
            | [] -> false
          do
            ()
          done
        with Conflict _ as e ->
          st.st_watched.(p) <- !l @ st.st_watched.(p) ;
          raise e
      done
    with Conflict _ as e ->
      Queue.clear st.st_prop_queue ;
      raise e

  (****)

  let raise_level st =
    st.st_cur_level <- st.st_cur_level + 1 ;
    st.st_trail_lim <- st.st_trail :: st.st_trail_lim ;
    st.st_trail <- []

  let assume st p =
    raise_level st ;
    enqueue st p None

  let protect st =
    propagate st ;
    raise_level st ;
    st.st_min_level <- st.st_cur_level

  let undo_one st p =
    let x = var_of_lit p in
    if !debug then Format.eprintf "Cancelling %a@." st.st_print_var x ;
    if st.st_assign.(x) = True then IntHash.remove st.st_assign_true x ;
    st.st_assign.(x) <- Unknown ;
    st.st_reason.(x) <- None ;
    st.st_level.(x) <- -1 ;
    List.iter
      (fun x ->
        charge st 1 ;
        st.st_refs.(x) <- st.st_refs.(x) - 1)
      st.st_associated_vars.(p) ;
    if st.st_refs.(x) > 0 && not st.st_pinned.(x) then enqueue_var st x

  let cancel st =
    st.st_cur_level <- st.st_cur_level - 1 ;
    List.iter (fun p -> undo_one st p) st.st_trail ;
    match st.st_trail_lim with
    | [] -> assert false
    | l :: r ->
        st.st_trail <- l ;
        st.st_trail_lim <- r

  let reset st =
    if !debug then Format.eprintf "Reset@." ;
    while st.st_trail_lim <> [] do
      cancel st
    done ;
    for i = 0 to Array.length st.st_refs - 1 do
      st.st_refs.(i) <- 0 ;
      st.st_pinned.(i) <- false
    done ;
    st.st_var_queue_head <- [] ;
    st.st_min_level <- 0 ;
    Queue.clear st.st_var_queue

  (****)

  let rec find_next_lit st =
    match st.st_trail with
    | [] -> assert false
    | p :: rem ->
        st.st_trail <- rem ;
        if st.st_seen_var.(var_of_lit p) = st.st_seen then (
          let reason = st.st_reason.(var_of_lit p) in
          undo_one st p ;
          (p, reason))
        else (
          undo_one st p ;
          find_next_lit st)

  let analyze st conflict =
    st.st_seen <- st.st_seen + 1 ;
    let counter = ref 0 in
    let learnt = ref [] in
    let bt_level = ref 0 in
    let reasons = ref [] in
    let r = ref conflict in
    while
      if !debug then (
        Array.iter
          (fun p ->
            Format.eprintf
              "%d:%a (%b/%d) "
              p
              print_val
              (val_of_lit st p)
              (st.st_reason.(var_of_lit p) <> None)
              st.st_level.(var_of_lit p))
          !r.lits ;
        Format.eprintf "@.") ;
      reasons := !r.reasons @ !reasons ;
      for i = 0 to Array.length !r.all_lits - 1 do
        let p = !r.all_lits.(i) in
        let x = var_of_lit p in
        if st.st_seen_var.(x) <> st.st_seen then (
          assert (val_of_lit st p = False) ;
          st.st_seen_var.(x) <- st.st_seen ;
          let level = st.st_level.(x) in
          if level = st.st_cur_level then incr counter
          else (
            (* if level > 0 then *)
            learnt := p :: !learnt ;
            bt_level := max level !bt_level))
      done ;
      let (p, reason) = find_next_lit st in
      decr counter ;
      (if !counter = 0 then learnt := lit_neg p :: !learnt
      else match reason with Some r' -> r := r' | None -> assert false) ;
      !counter > 0
    do
      ()
    done ;
    if !debug then (
      List.iter
        (fun p ->
          Format.eprintf
            "%d:%a/%d "
            p
            print_val
            (val_of_lit st p)
            st.st_level.(var_of_lit p))
        !learnt ;
      Format.eprintf "@.") ;
    (Array.of_list !learnt, !reasons, !bt_level)

  let find_highest_level st lits =
    let level = ref (-1) in
    let i = ref 0 in
    Array.iteri
      (fun j p ->
        if st.st_level.(var_of_lit p) > !level then (
          level := st.st_level.(var_of_lit p) ;
          i := j))
      lits ;
    !i

  let backjump f st r =
    let (learnt, reasons, level) = analyze st r in
    let level = max st.st_min_level level in
    while st.st_cur_level > level do
      cancel st
    done ;
    assert (val_of_lit st learnt.(0) = Unknown) ;
    let rule = { lits = learnt; all_lits = learnt; reasons } in
    if !debug then Format.eprintf "Learning %a@." (print_rule st) rule ;
    if Array.length learnt > 1 then (
      let i = find_highest_level st learnt in
      assert (i > 0) ;
      let p' = learnt.(i) in
      learnt.(i) <- learnt.(1) ;
      learnt.(1) <- p' ;
      let p = lit_neg learnt.(0) in
      let p' = lit_neg p' in
      st.st_watched.(p) <- rule :: st.st_watched.(p) ;
      st.st_watched.(p') <- rule :: st.st_watched.(p')) ;
    enqueue st learnt.(0) (Some rule) ;
    st.st_cur_level > st.st_min_level && f st

  (*
  let val_of = function
    |True -> true
    |False -> false
    |Unknown -> assert false
*)

  (* find all solutions *)
  let rec solve_all_rec callback st =
    match
      try
        propagate st ;
        None
      with Conflict r -> Some r
    with
    | None ->
        let x = dequeue_var st in
        if x < 0 then (
          (* we do something with the solution that we just found *)
          callback st ;
          if st.st_cur_level = 0 then (
            (* we exhausted the search space *)
            if !debug then Format.eprintf "Search Completed.@." ;
            true)
          else (
            if !debug then Format.eprintf "Solution found.@." ;
            (* we remove this solution from the search space and backjump *)
            let assignment =
              (* XXX : I should keep trace of this list incrementally *)
              let acc = ref [] in
              for v = 0 to Array.length st.st_assign - 1 do
                match st.st_assign.(v) with
                | True -> acc := lit_of_var v true :: !acc
                | False -> acc := lit_of_var v false :: !acc
                | Unknown -> ()
              done ;
              !acc
            in
            let m = Array.of_list (List.map lit_neg assignment) in
            let r = { lits = m; all_lits = m; reasons = [] } in
            backjump (solve_all_rec callback) st r))
        else (
          (* we didn't find any solution yet *)
          assume st (lit_of_var x false) ;
          solve_all_rec callback st)
    | Some r ->
        let r = match r with None -> assert false | Some r -> r in
        (* we found a conflict *)
        backjump (solve_all_rec callback) st r

  (* find one solution *)
  let rec solve_rec st =
    match
      try
        propagate st ;
        None
      with Conflict r -> Some r
    with
    | None ->
        let x = dequeue_var st in
        x < 0
        ||
        (assume st (lit_of_var x false) ;
         solve_rec st)
    | Some r ->
        let r = match r with None -> assert false | Some r -> r in
        backjump solve_rec st r

  let rec solve_aux ?callback st x =
    let s =
      if Option.is_none callback then solve_rec
      else solve_all_rec (Option.get callback)
    in
    assert (st.st_cur_level = st.st_min_level) ;
    propagate st ;
    try
      let p = lit_of_var x true in
      assume st p ;
      assert (st.st_cur_level = st.st_min_level + 1) ;
      if s st then (
        protect st ;
        true)
      else solve_aux st ?callback x
    with Conflict _ ->
      false

  let solve st x = solve_aux st x

  let rec solve_lst_rec st l0 l =
    match l with
    | [] -> true
    | x :: r ->
        protect st ;
        List.iter (fun x -> enqueue st (lit_of_var x true) None) l0 ;
        propagate st ;
        if solve st x then (
          if r <> [] then reset st ;
          solve_lst_rec st (x :: l0) r)
        else false

  let solve_lst st l = solve_lst_rec st [] l

  let initialize_problem n =
    (* Remove Gc settings for the moment as they are not adapted to small
          opam repositories
       Gc.set { (Gc.get()) with
         Gc.minor_heap_size = 4 * 1024 * 1024; (*4M*)
         Gc.major_heap_increment = 32 * 1024 * 1024; (*32M*)
         Gc.max_overhead = 150;
       } ;
    *)
    { st_assign = Array.make n Unknown;
      st_assign_true = IntHash.create n;
      st_reason = Array.make n None;
      st_level = Array.make n (-1);
      st_seen_var = Array.make n (-1);
      st_refs = Array.make n 0;
      st_pinned = Array.make n false;
      (* to each literal, positive or negative,
       * we associate the list of rules where it appears *)
      st_simpl_prop = Array.make (2 * n) LitMap.empty;
      st_watched = Array.make (2 * n) [];
      (* to each literal we associate the list of assiciated variables *)
      st_associated_vars = Array.make (2 * n) [];
      st_trail = [];
      st_trail_lim = [];
      st_prop_queue = Queue.create ();
      st_cur_level = 0;
      st_min_level = 0;
      st_seen = 0;
      st_var_queue_head = [];
      st_var_queue = Queue.create ();
      st_cost = 0;
      st_print_var = (fun fmt -> Format.fprintf fmt "%d");
    }

  let insert_simpl_prop st r p p' =
    let p = lit_neg p in
    if not (LitMap.mem p' st.st_simpl_prop.(p)) then
      st.st_simpl_prop.(p) <- LitMap.add p' r st.st_simpl_prop.(p)

  let add_bin_rule st lits p p' reasons =
    let r = { lits = [| p; p' |]; all_lits = lits; reasons } in
    insert_simpl_prop st r p p' ;
    insert_simpl_prop st r p' p

  let add_un_rule st lits p reasons =
    let r = { lits = [| p |]; all_lits = lits; reasons } in
    enqueue st p (Some r)

  let add_rule st lits reasons =
    let is_true = ref false in
    let all_lits = Array.copy lits in
    let j = ref 0 in
    for i = 0 to Array.length lits - 1 do
      match val_of_lit st lits.(i) with
      | True -> is_true := true
      | False -> ()
      | Unknown ->
          lits.(!j) <- lits.(i) ;
          incr j
    done ;
    let lits = Array.sub lits 0 !j in
    if not !is_true then
      match Array.length lits with
      | 0 -> assert false
      | 1 -> add_un_rule st all_lits lits.(0) reasons
      | 2 -> add_bin_rule st all_lits lits.(0) lits.(1) reasons
      | _ ->
          let rule = { lits; all_lits; reasons } in
          let p = lit_neg rule.lits.(0) in
          let p' = lit_neg rule.lits.(1) in
          assert (val_of_lit st p <> False) ;
          assert (val_of_lit st p' <> False) ;
          st.st_watched.(p) <- rule :: st.st_watched.(p) ;
          st.st_watched.(p') <- rule :: st.st_watched.(p')

  let associate_vars st lit l =
    st.st_associated_vars.(lit) <- l @ st.st_associated_vars.(lit)

  let rec collect_rec st x l =
    if st.st_seen_var.(x) = st.st_seen then l
    else (
      st.st_seen_var.(x) <- st.st_seen ;
      match st.st_reason.(x) with
      | None -> l
      | Some r ->
          r.reasons
          @ Array.fold_left
              (fun l p -> collect_rec st (var_of_lit p) l)
              l
              r.all_lits)

  let collect_reasons st x =
    st.st_seen <- st.st_seen + 1 ;
    collect_rec st x []

  let collect_reasons_lst st l =
    st.st_seen <- st.st_seen + 1 ;
    let x = List.find (fun x -> st.st_assign.(x) = False) l in
    collect_rec st x []

  let assignment_true st =
    IntHash.fold (fun k _ acc -> k :: acc) st.st_assign_true []
end
end

module Util = struct
(* ExtList.remove_if *)
let rec list_remove_if f = function
  | [] -> []
  | x::xs when f x -> xs
  | x::xs -> x :: list_remove_if f xs

let fatal fmt =
  Printf.ksprintf
    (fun s ->
       Printf.eprintf "FATAL ERROR: %s\n%!" s ;
       Stdlib.exit 64)
    fmt

module IntHashtbl = Hashtbl.Make (struct
  type t = int

  let equal (a : int) (b : int) = a = b

  let hash i = Hashtbl.hash i
end)

module IntPairHashtbl = Hashtbl.Make (struct
  type t = int * int

  let equal (a : int * int) (b : int * int) = a = b

  let hash i = Hashtbl.hash i
end)

class type projection =
  object
    method inttovar : int -> int

    method vartoint : int -> int
  end

(** associate a sat solver variable to a package id *)
class intprojection size =
  object
    val vartoint = IntHashtbl.create (2 * size)

    val inttovar = Array.make size 0

    val mutable counter = 0

    (** add a package id to the map *)
    method add v =
      if size = 0 then assert false ;
      if counter > size - 1 then assert false ;
      IntHashtbl.add vartoint v counter ;
      inttovar.(counter) <- v ;
      counter <- counter + 1

    (** given a package id return a sat solver variable
      raise Not_found if the package id is not known *)
    method vartoint v = IntHashtbl.find vartoint v

    (* given a sat solver variable return a package id *)
    method inttovar i =
      if i >= size then fatal "out of boundary i = %d size = %d" i size ;
      inttovar.(i)
  end

class identity =
  object
    method vartoint (v : int) = v
    method inttovar (v : int) = v
  end
end

type reason_int =
  | DependencyInt of (int * Cudf_types.vpkg list * int list)
  | MissingInt of (int * Cudf_types.vpkg list)
  | ConflictInt of (int * int * Cudf_types.vpkg)

type result_int =
  | SuccessInt of (unit -> int list)
  | FailureInt of (unit -> reason_int list)

(** One un-installability reason for a package *)
type reason =
  | Dependency of (Cudf.package * Cudf_types.vpkg list * Cudf.package list)
      (** Not strictly a un-installability, Dependency (a,vpkglist,pkglist) is used
      to recontruct the the dependency path from the root package to the
      offending un-installable package *)
  | Missing of (Cudf.package * Cudf_types.vpkg list)
      (** Missing (a,vpkglist) means that the dependency
      [vpkglist] of package [a] cannot be satisfied *)
  | Conflict of (Cudf.package * Cudf.package * Cudf_types.vpkg)
      (** Conflict (a,b,vpkg) means that the package [a] is in conflict
      with package [b] because of vpkg *)

(** The request provided to the solver.
    Check the installability of one package or the
    coinstallability of a list of packages *)
type request = Cudf.package list

(** The result of an installability query *)
type result =
  | Success of (unit -> Cudf.package list)
      (** If successfull returns a function that will
      return the installation set for the given query. Since
      not all packages are tested for installability directly, the
      installation set might be empty. In this case, the solver can
      be called again to provide the real installation set
      using the parameter [~all:true] *)
  | Failure of (unit -> reason list)
      (** If unsuccessful returns a function containing the list of reason *)

type diagnosis = { result : result; request : request }

let reason map universe =
  let from_sat = Cudf.package_by_uid universe in
  let globalid = map#vartoint (Cudf.universe_size universe) in
  List.filter_map (function
      | DependencyInt (i, _vl, _il) when i = globalid -> None
      | MissingInt (i, _vl) when i = globalid ->
          Util.fatal
            "the package encoding global constraints can't be missing (uid %d)"
            i
      | ConflictInt (i, j, _vpkg) when i = globalid || j = globalid ->
          Util.fatal
            "the package encoding global constraints can't be in conflict (uid \
             %d - %d)"
            i
            j
      | DependencyInt (i, vl, il) ->
          Some
            (Dependency
               ( from_sat (map#inttovar i),
                 vl,
                 List.map (fun i -> from_sat (map#inttovar i)) il ))
      | MissingInt (i, vl) -> Some (Missing (from_sat (map#inttovar i), vl))
      | ConflictInt (i, j, vpkg) ->
          Some
            (Conflict
               (from_sat (map#inttovar i), from_sat (map#inttovar j), vpkg)))

let result map universe result =
  let from_sat = Cudf.package_by_uid universe in
  let globalid = map#vartoint (Cudf.universe_size universe) in
  match result with
  | SuccessInt f_int ->
      Success
        (fun () ->
          List.filter_map
            (function
              | i when i = globalid -> None
              | i ->
                  Some
                    { (from_sat (map#inttovar i)) with Cudf.installed = true })
            (f_int ()))
  | FailureInt f -> Failure (fun () -> reason map universe (f ()))

let request universe result = List.map (Cudf.package_by_uid universe) result

(* XXX here the threatment of result and request is not uniform.
 * On one hand indexes in result must be processed with map#inttovar
 * as they represent indexes associated with the solver.
 * On the other hand the indexes in result represent cudf uid and
 * therefore do not need to be processed.
 * Ideally the compiler should make sure that we use the correct indexes
 * but we should annotate everything making packing/unpackaing handling
 * a bit too heavy *)
let diagnosis map universe res req =
  let result = result map universe res in
  let request = request universe req in
  { result; request }

let get_installationset = function
  | { result = Success f; _ } -> f ()
  | { result = Failure _; _ } -> raise Not_found

module Depsolver_int = struct
module S = EdosSolver.M (struct type reason = reason_int end)

type solver =
  { constraints : S.state;
    map : Util.projection;
    globalid : (bool * bool) * int
  }

(* cudf uid -> cudf uid array . Here we assume cudf uid are sequential
   and we can use them as an array index *)
let init_pool_univ univ =
  (* the last element of the array *)
  let size = Cudf.universe_size univ in
  let keep = Hashtbl.create 200 in
  let add_to_package_list n p =
    try
      let l = Hashtbl.find keep n in
      l := p :: !l
    with Not_found -> Hashtbl.add keep n (ref [p])
  in
  let pool =
    (* here I initalize the pool to size + 1, that is I reserve one spot
     * to encode the global constraints associated with the universe.
     * However, since they are global, I've to add the at the end, after
     * I have analyzed all packages in the universe. *)
    Array.init (size + 1) (fun uid ->
        try
          if uid = size then ([], []) (* the last index *)
          else
            let pkg = Cudf.package_by_uid univ uid in
            let dll =
              List.map
                (fun vpkgs -> (vpkgs, CudfAdd.resolve_vpkgs_int univ vpkgs))
                pkg.Cudf.depends
            in
            let cl =
              List.filter_map
                (fun vpkg ->
                  match CudfAdd.resolve_vpkg_int univ vpkg with
                  | [] -> None
                  | l -> Some (vpkg, l))
                pkg.Cudf.conflicts
            in
            (if pkg.Cudf.installed then
             match pkg.Cudf.keep with
             | `Keep_none -> ()
             | `Keep_package ->
                 List.iter
                   (fun id ->
                     add_to_package_list
                       (pkg.Cudf.package, None)
                       id)
                   (CudfAdd.resolve_vpkg_int univ (pkg.Cudf.package, None))
             | `Keep_version ->
                 add_to_package_list
                   (pkg.Cudf.package, Some (`Eq, pkg.Cudf.version))
                   uid
             | `Keep_feature ->
                 List.iter
                   (function
                     | (name, None) ->
                         List.iter
                           (fun id -> add_to_package_list (name, None) id)
                           (CudfAdd.resolve_vpkg_int univ (name, None))
                     | (name, Some (`Eq, v)) ->
                         List.iter
                           (fun id ->
                             add_to_package_list (name, Some (`Eq, v)) id)
                           (CudfAdd.resolve_vpkg_int univ (name, Some (`Eq, v))))
                   pkg.Cudf.provides) ;
            (dll, cl)
        with Not_found ->
          Util.fatal
            "Package uid (%d) not found during solver pool initialization. \
             Packages uid must have no gaps in the given universe"
            uid)
  in
  let keep_dll =
    Hashtbl.fold
      (fun cnstr { contents = l } acc -> ([cnstr], l) :: acc)
      keep []
  in
  pool.(size) <- (keep_dll, []) ;
  `CudfPool (keep_dll <> [], pool)

(** this function creates an array indexed by solver ids that can be
    used to init the edos solver *)
let init_solver_pool map (`CudfPool (_keep_constraints, cudfpool)) closure =
  let convert (dll, cl) =
    let sdll =
      List.map (fun (vpkgs, uidl) -> (vpkgs, List.map map#vartoint uidl)) dll
    in
    let scl =
      (* ignore conflicts that are not in the closure.
       * if nobody depends on a conflict package, then it is irrelevant.
       * This requires a leap of faith in the user ability to build an
       * appropriate closure. If the closure is wrong, you are on your own *)
      List.map
        (fun (vpkg, uidl) ->
          let l =
            List.filter_map
              (fun uid ->
                try Some (map#vartoint uid)
                with Not_found -> None)
              uidl
          in
          (vpkg, l))
        cl
    in
    (sdll, scl)
  in
  let solverpool =
    Array.init (List.length closure) (fun sid ->
        convert cudfpool.(map#inttovar sid))
  in
  `SolverPool solverpool

(** initalise the sat solver. operate only on solver ids *)
let init_solver_cache ?(explain = true) (`SolverPool varpool)
    =
  let num_conflicts = ref 0 in
  let num_disjunctions = ref 0 in
  let num_dependencies = ref 0 in
  let if_explain l = if explain then l else [] in
  let varsize = Array.length varpool in
  let add_depend constraints vpkgs pkg_id l =
    let lit = S.lit_of_var pkg_id false in
    if List.length l = 0 then
      S.add_rule
        constraints
        [| lit |]
        (if_explain [MissingInt (pkg_id, vpkgs)])
    else
      let lits = List.map (fun id -> S.lit_of_var id true) l in
      num_disjunctions := !num_disjunctions + List.length lits ;
      S.add_rule
        constraints
        (Array.of_list (lit :: lits))
        (if_explain [DependencyInt (pkg_id, vpkgs, l)]) ;
      if List.length lits > 1 then
        S.associate_vars constraints (S.lit_of_var pkg_id true) l
  in
  let conflicts = Util.IntPairHashtbl.create (varsize / 10) in
  let add_conflict constraints vpkg (i, j) =
    if i <> j then
      let pair = (min i j, max i j) in
      (* we get rid of simmetric conflicts *)
      if not (Util.IntPairHashtbl.mem conflicts pair) then (
        incr num_conflicts ;
        Util.IntPairHashtbl.add conflicts pair () ;
        let p = S.lit_of_var i false in
        let q = S.lit_of_var j false in
        S.add_rule
          constraints
          [| p; q |]
          (if_explain [ConflictInt (i, j, vpkg)]))
  in
  let exec_depends constraints pkg_id dll =
    List.iter
      (fun (vpkgs, dl) ->
        incr num_dependencies ;
        add_depend constraints vpkgs pkg_id dl)
      dll
  in
  let exec_conflicts constraints pkg_id cl =
    List.iter
      (fun (vpkg, l) ->
        List.iter (fun id -> add_conflict constraints vpkg (pkg_id, id)) l)
      cl
  in
  let constraints = S.initialize_problem varsize in
  Array.iteri
    (fun id (dll, cl) ->
      exec_depends constraints id dll ;
      exec_conflicts constraints id cl)
    varpool ;
  Util.IntPairHashtbl.clear conflicts ;
  S.propagate constraints ;
  constraints

(** low level call to the sat solver

    @param tested: optional int array used to cache older results
*)
let solve ~tested ~explain solver request =
  S.reset solver.constraints ;
  let result solve collect var =
    (* Real call to the SAT solver *)
    if solve solver.constraints var then
      if explain then (
        let l = S.assignment_true solver.constraints in
        if not (Option.is_none tested) then
          List.iter (fun i -> (Option.get tested).(i) <- true) l ;
        SuccessInt (fun () -> l))
      else (
        (if not (Option.is_none tested) then
         let l = S.assignment_true solver.constraints in
         List.iter (fun i -> (Option.get tested).(i) <- true) l) ;
        SuccessInt (fun () -> []))
    else if explain then
      FailureInt (fun () -> collect solver.constraints var)
    else FailureInt (fun () -> [])
  in
  match (request, solver.globalid) with
  | ([], ((false, false), _)) ->
      SuccessInt (fun () -> [])
  | ([], (((_, true) | (true, _)), gid)) ->
      result S.solve S.collect_reasons (solver.map#vartoint gid)
  | ([i], ((false, false), _)) ->
      result S.solve S.collect_reasons (solver.map#vartoint i)
  | (l, ((false, false), _)) ->
      let il = List.map solver.map#vartoint l in
      result S.solve_lst S.collect_reasons_lst il
  | (l, (_, gid)) ->
      let il = List.map solver.map#vartoint (gid :: l) in
      result S.solve_lst S.collect_reasons_lst il

(* this function is used to "distcheck" a list of packages. The id is a cudfpool index *)
let pkgcheck callback solver tested id =
  let res =
    if not tested.(id) then solve ~tested:(Some tested) ~explain:false solver [id]
    else
      (* this branch is true only if the package was previously
         added to the tested packages and therefore it is installable
         if all = true then the solver is called again to provide the list
         of installed packages despite the fact the the package was already
         tested. This is done to provide one installation set for each package
         in the universe *)
      SuccessInt (fun () -> [])
  in
  callback (res, [id]) ;
  match res with
  | SuccessInt _ -> true
  | FailureInt _ -> false

(** low level constraint solver initialization

    @param univ cudf package universe
*)
let init_solver_univ univ =
  let map = new Util.identity in
  (* here we convert a cudfpool in a varpool. The assumption
   * that cudf package identifiers are contiguous is essential ! *)
  let (`CudfPool (keep_constraints, pool)) =
    init_pool_univ univ
  in
  let varpool = `SolverPool pool in
  let constraints = init_solver_cache ~explain:false varpool in
  let gid = Cudf.universe_size univ in
  { constraints; map; globalid = ((keep_constraints, false), gid) }

(* pool = cudf pool - closure = dependency clousure . cudf uid list *)

(** low level constraint solver initialization

    @param buffer debug buffer to print out debug messages
    @param pool dependencies and conflicts array idexed by package id
    @param closure subset of packages used to initialize the solver
*)
let init_solver_closure
    (`CudfPool (keep_constraints, cudfpool)) closure =
  let gid = Array.length cudfpool - 1 in
  let map = new Util.intprojection (List.length closure) in
  List.iter map#add closure ;
  let varpool =
    init_solver_pool map (`CudfPool (keep_constraints, cudfpool)) closure
  in
  let constraints = init_solver_cache varpool in
  { constraints; map = (map :> Util.projection); globalid = ((keep_constraints, false), gid) }

(***********************************************************)

let dependency_closure_cache (`CudfPool (_, cudfpool)) idlist =
  let queue = Queue.create () in
  let globalid = Array.length cudfpool - 1 in
  let visited = Hashtbl.create (2 * List.length idlist) in
  List.iter
    (fun e -> Queue.add (e, 0) queue)
    (CudfAdd.normalize_set (globalid :: idlist)) ;
  while Queue.length queue > 0 do
    let (id, level) = Queue.take queue in
    if (not (Hashtbl.mem visited id)) && level < max_int then (
      Hashtbl.add visited id () ;
      let (l, _) = cudfpool.(id) in
      List.iter
        (fun (_, dsj) ->
              List.iter
                (fun i ->
                  if not (Hashtbl.mem visited i) then
                    Queue.add (i, level + 1) queue)
                dsj)
        l)
  done ;
  Hashtbl.fold (fun k _ l -> k :: l) visited []

(*    XXX : elements in idlist should be included only if because
 *    of circular dependencies *)
end

(** [listcheck ?callback universe pkglist] check if a subset of packages
    un the universe are installable.

    @param pkglist list of packages to be checked
    @return the number of packages that cannot be installed
*)
let listcheck ~callback universe pkglist =
  let aux ~callback univ idlist =
    let solver = Depsolver_int.init_solver_univ univ in
    let failed = ref 0 in
    let size = Cudf.universe_size univ + 1 in
    let tested = Array.make size false in
    let check = Depsolver_int.pkgcheck callback solver tested in
    (match fst solver.Depsolver_int.globalid with
    | (false, false) ->
        List.iter (fun id -> if not (check id) then incr failed) idlist
    | _ ->
        let gid = snd solver.Depsolver_int.globalid in
        List.iter
          (function
            | id when id = gid -> () | id -> if not (check id) then incr failed)
          idlist) ;
    !failed
  in
  let idlist = List.map (Cudf.uid_by_package universe) pkglist in
  let map = new Util.identity in
  let callback_int (res, req) =
    callback (diagnosis map universe res req)
  in
  aux ~callback:callback_int universe idlist

let edos_install_cache univ cudfpool pkglist =
  let idlist = List.map (Cudf.uid_by_package univ) pkglist in
  let closure = Depsolver_int.dependency_closure_cache cudfpool idlist in
  let solver =
    Depsolver_int.init_solver_closure cudfpool closure
  in
  let res = Depsolver_int.solve solver ~tested:None ~explain:true idlist in
  diagnosis solver.Depsolver_int.map univ res idlist

let edos_install universe pkg =
  let cudfpool = Depsolver_int.init_pool_univ universe in
  edos_install_cache universe cudfpool [pkg]

let edos_coinstall universe pkglist =
  let cudfpool = Depsolver_int.init_pool_univ universe in
  edos_install_cache universe cudfpool pkglist

type solver_result_sat = (Cudf.preamble option * Cudf.universe)
type solver_result =
  | Sat of solver_result_sat
  | Unsat of diagnosis option

let dummy_request =
  { Cudf.default_package with Cudf.package = "dose-dummy-request"; version = 1 }

(* add a version constraint to ensure name is upgraded *)
let upgrade_constr universe name =
  match Cudf.get_installed universe name with
  | [] -> (name, None)
  | [p] -> (name, Some (`Geq, p.Cudf.version))
  | pl ->
      let p = List.hd (List.sort Cudf.( >% ) pl) in
      (name, Some (`Geq, p.Cudf.version))

let add_dummy universe request dummy =
  let deps =
    let il = request.Cudf.install in
    (* we preserve the user defined constraints, while adding the upgrade constraint *)
    let ulc =
      List.filter
        (function (_, Some _) -> true | _ -> false)
        request.Cudf.upgrade
    in
    let ulnc =
      List.map
        (fun (name, _) -> upgrade_constr universe name)
        request.Cudf.upgrade
    in
    let l = il @ ulc @ ulnc in
    List.map (fun j -> [j]) l
  in
  let dummy =
    { dummy with
      Cudf.depends = deps @ dummy.Cudf.depends;
      conflicts = request.Cudf.remove @ dummy.Cudf.conflicts
    }
  in
  (* XXX it should be possible to add a package to a cudf document ! *)
  let pkglist = Cudf.get_packages universe in
  let universe = Cudf.load_universe (dummy :: pkglist) in
  (universe, dummy)

let remove_dummy pre (dummy, d) =
  match d with
  | {result = Success _; _} ->
    let is =
      Util.list_remove_if (Cudf.( =% ) dummy) (get_installationset d)
    in
    Sat (Some pre, Cudf.load_universe is)
  | {result = Failure _; _} ->
    Unsat (Some d)

let check_request_using ~call_solver (pre, universe, request) =
  match call_solver with
  | None ->
      let (u, r) = add_dummy universe request dummy_request in
      remove_dummy pre (r, edos_install u r)
  | Some call_solver -> (
      try
        Sat (call_solver (pre, universe, request))
      with
      | Unsat ->
          let (u, r) = add_dummy universe request dummy_request in
          remove_dummy pre (r, edos_install u r))
