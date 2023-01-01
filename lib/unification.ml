open Href

module type Term = sig
  type term
  type 'a uterm

  val union : ('a -> 'a -> bool) -> 'a uterm -> 'a uterm -> bool
  val build : ('a -> term option) -> 'a uterm -> term option
  val args : 'a uterm -> 'a list
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

  let rec not_in_fv (args : var Unit.uterm -> 'a list) (x : var)
      (term : var Unit.uterm) =
    List.for_all
      (fun v ->
        x != v
        && Option.fold ~none:true
             ~some:(fun t -> not_in_fv args x t)
             (get_value v))
      (args term)

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
            (Some x, not_in_fv Unit.args vv x)
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
      | Some x, Some y when Unit.union equal x y -> Some x
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