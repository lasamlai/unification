open Backtracking_references
open Term

module type Var = sig
  type state

  exception UseBeforeCreation

  val get_current : unit -> state
  val checkpoint : unit -> state
  val fail : state -> unit
  val cut : state -> unit

  type term
  (** Term description without unifying-variables. *)

  type 'a uterm
  (** Description of terms with unifying variables. *)

  type var
  (** The type of variables. *)

  type t = var
  (** The type of variables, but here named as [t] to follow ocaml convention. *)

  val gen_var : unit -> var
  (** [gen_var ()] creates a new free variable. *)

  val var_of_uterm : var uterm -> var
  (** [var_of_uterm u] creates a new variable, which is unified with the term [u]. *)

  val var_of_term : term -> var
  (** [var_of_term t] creates a new variable, which is unified with the term [t]. *)

  val union : var -> var -> bool
  (** [union x1 x2] tries to unify the variables [x1] and [x2] and returns [true] if successful, [false] otherwise. *)

  val equal : var -> var -> bool
  (** [equal x1 x2] is [true] if variables are equal, [false] otherwise.

      {b Note}: Two variables are equal if they contain equal terms or have been unified.
      This should work like [==/2] in the Prolog.

      {b Warning}: The information that two terms are equal will be cached in the [Unification] structure. *)

  val is_var : var -> bool
  (** [is_var x] is [true] if the variable [x] contains no term, [false] otherwise.

      {b Note}: This should work like [var/1] in Prolog. *)

  val set_value : var -> var uterm -> bool
  (** [set_value x u] is [union x (var_of_uterm u)]. *)

  val get_value : var -> var uterm option
  (** [get_value x] is [Some u] if variable contains term, [None] otherwise. *)

  val get : var -> term option
  (** [get x] is [Some t] if variable contains grounded term, [None] otherwise. *)
end

module Var (Unit : Term) :
  Var with type term = Unit.term and type 'a uterm = 'a Unit.uterm = struct
  include Backtracking_references

  type term = Unit.term
  type 'a uterm = 'a Unit.uterm

  type node = Top of int * var Unit.uterm option | Link of var
  and var = node href

  type t = var

  let gen_var () : var = href (Top (1, None))
  let var_of_uterm t : var = href (Top (1, Some t))

  let rec var_of_term t : var =
    var_of_uterm @@ Unit.map var_of_term (Unit.unwrap t)

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

  let rec not_in_fv (x : var) (term : var Unit.uterm) =
    List.for_all
      (fun v ->
        let v, _, _ = find v in
        x != v
        && Option.fold ~none:true ~some:(fun t -> not_in_fv x t) (get_value v))
      (Unit.children_of term)

  let union v1 v2 : bool =
    let rec union v1 v2 : bool =
      let g1, size1, oval1 = find v1 in
      let g2, size2, oval2 = find v2 in
      g1 == g2
      ||
      let oval, unified =
        match (g1, oval1, g2, oval2) with
        | _, None, _, None -> (None, true)
        | _, Some x, vv, None | vv, None, _, Some x -> (Some x, not_in_fv vv x)
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

  let set_value var v = union var (var_of_uterm v)

  let rec get (var : var) : Unit.term option =
    match get_value var with Some t -> Unit.build get t | None -> None
end
