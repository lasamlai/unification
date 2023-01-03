open Href

module type Term = sig
  type term
  (** Term description without unifying-variables. *)

  type 'a uterm
  (** Description of terms with unifying variables. *)

  val children_of : 'a uterm -> 'a list
  (** `children_of t` is a list of children of term `t`. *)

  val build : ('a -> term option) -> 'a uterm -> term option
  (** `build f ut` is `Some t` if `ut` is a grounded term and `None` otherwise.
   *  `t` is grounded version of term `ut`.
   *  `f x` is the term assigned to the variable `x` if `x` is grounded. *)

  val union : ('a -> 'a -> bool) -> 'a uterm -> 'a uterm -> bool
  (** `union u t1 t2` tries to unify the terms `t1` and `t2` and returns `true` if successful, `false` otherwise.
   *  `u x1 x2` tries to unify the variables `x1` and `x2` and returns `true` if successful, `false` otherwise. *)

  val equal : ('a -> 'a -> bool) -> 'a uterm -> 'a uterm -> bool
  (** `equal eq t1 t2` is `true` if terms are equal, `false` otherwise.
   *  `eq x1 x2` is `true` if the variables are equal, `false` otherwise.
   *
   *  Note: Two variables are equal if they contain equal terms or have been unified.
   *  This should work like `==/2` in the Prolog.
   *
   *  Warning: The information that two terms are equal will be cached in the `Unification` structure. *)
end

module Unification (Unit : Term) : sig
  type state

  exception UseBeforeCreation

  val get_current : unit -> state
  val checkpoint : unit -> state
  val fail : state -> unit
  val cut : state -> unit

  type var

  val gen_var : unit -> var
  val union : var -> var -> bool
  val equal : var -> var -> bool
  val is_var : var -> bool
  val set_value : var -> var Unit.uterm -> bool
  val get_value : var -> var Unit.uterm option
  val get : var -> Unit.term option
end = struct
  include Href

  type node = Top of int * var Unit.uterm option | Link of var
  and var = node href

  let gen_var () : var = href (Top (1, None))

  let rec find (v : var) : var * int * var Unit.uterm option =
    match !v with
    | Top (size, oval) -> (v, size, oval)
    | Link lnk ->
        let ((bos, _, _) as fnd) = find lnk in
        v := Link bos;
        fnd

  let get_value (v : var) : var Unit.uterm option =
    let _, _, oval = find v in
    oval

  let rec not_in_fv (children_of : var Unit.uterm -> 'a list) (x : var)
      (term : var Unit.uterm) =
    List.for_all
      (fun v ->
        x != v
        && Option.fold ~none:true
             ~some:(fun t -> not_in_fv children_of x t)
             (get_value v))
      (children_of term)

  let union v1 v2 : bool =
    let rec union v1 v2 : bool =
      let g1, size1, oval1 = find v1 in
      let g2, size2, oval2 = find v2 in
      g1 == g2
      ||
      let oval, unified =
        match (g1, oval1, g2, oval2) with
        | _, None, _, None -> (None, true)
        | _, Some x, vv, None | vv, None, _, Some x ->
            (Some x, not_in_fv Unit.children_of vv x)
        | _, Some x, _, Some y -> (Some x, Unit.union union x y)
      in
      if unified then (
        let size = size1 + size2 in
        let g1, g2 = if size1 <= size2 then (g1, g2) else (g2, g1) in
        g2 := Top (size, oval);
        g1 := Link g2);
      unified
    in
    let ckp = checkpoint () in
    let unified = union v1 v2 in
    if unified then cut ckp else fail ckp;
    unified

  let rec equal v1 v2 =
    let g1, size1, oval1 = find v1 in
    let g2, size2, oval2 = find v2 in
    g1 == g2
    ||
    let connected =
      match (oval1, oval2) with
      | Some x, Some y when Unit.equal equal x y -> Some x
      | _ -> None
    in
    let equal = connected <> None in
    if equal then (
      let size = size1 + size2 in
      let g1, g2 = if size1 <= size2 then (g1, g2) else (g2, g1) in
      g2 := Top (size, connected);
      g1 := Link g2);
    equal

  let is_var var =
    let _, _, oval = find var in
    oval = None

  let set_value var v =
    match find var with
    | idx, size, None ->
        idx := Top (size, Some v);
        true
    | _, _, Some value -> Unit.union union value v

  let rec get (var : var) : Unit.term option =
    match get_value var with Some t -> Unit.build get t | None -> None
end
