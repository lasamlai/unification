Unification: Variables, unification and backtracking like in Prolog.
====================================================================

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https://ci.ocamllabs.io/badge/lasamlai/unification/master&logo=ocaml)](https://ci.ocamllabs.io/github/lasamlai/unification)

Implementation of the unification algorithm with backtracking.

Example
-------

A simple example of how to add unification to a term.

First, create a description of your term:

```ocaml
module MyTerm = struct
  (** Term description without unifying-variables. *)
  type term = Var of string | Impl of term * term | Box of term

  (** Description of terms with unifying variables. *)
  type 'a uterm = UVar of string | UImpl of 'a * 'a | UBox of 'a

  (** `children_of t` is a list of children of term `t`. *)
  let children_of (t : 'a uterm) : 'a list =
    match t with UVar _ -> [] | UImpl (t1, t2) -> [ t1; t2 ] | UBox t -> [ t ]

  (** `build f ut` is `Some t` if `ut` is a grounded term and `None` otherwise.
   *  `t` is grounded version of term `ut`.
   *  `f x` is the term assigned to the variable `x` if `x` is grounded. *)
  let build get (t : 'a uterm) : term option =
    match t with
    | UVar s -> Some (Var s)
    | UImpl (t1, t2) -> (
        match (get t1, get t2) with
        | Some t1, Some t2 -> Some (Impl (t1, t2))
        | _ -> None)
    | UBox t -> ( match get t with Some t -> Some (Box t) | None -> None)

  (** `union u t1 t2` tries to unify the terms `t1` and `t2` and returns `true` if successful, `false` otherwise.
   *  `u x1 x2` tries to unify the variables `x1` and `x2` and returns `true` if successful, `false` otherwise. *)
  let union union ta tb =
    match (ta, tb) with
    | UVar sa, UVar sb -> sa = sb
    | UImpl (ta1, ta2), UImpl (tb1, tb2) -> union ta1 tb1 && union ta2 tb2
    | UBox ta, UBox tb -> union ta tb
    | _ -> false

  (** `equal u t1 t2` is `true` if terms are equal, `false` otherwise.
   *  `u x1 x2` is `true` if the variables are unified, `false` otherwise.
   *  This should work like `==/2` in the Prolog.
   *  (See: https://www.swi-prolog.org/pldoc/doc_for?object=(%3D%3D)/2) *)
  let equal equal ta tb =
    match (ta, tb) with
    | UVar sa, UVar sb -> sa = sb
    | UImpl (ta1, ta2), UImpl (tb1, tb2) -> equal ta1 tb1 && equal ta2 tb2
    | UBox ta, UBox tb -> equal ta tb
    | _ -> false
end
```

Your description should be of the following type:

```ocaml
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
  (** `equal u t1 t2` is `true` if terms are equal, `false` otherwise.
   *  `u x1 x2` is `true` if the variables are unified, `false` otherwise.
   *  This should work like `==/2` in the Prolog.
   *  (See: https://www.swi-prolog.org/pldoc/doc_for?object=(%3D%3D)/2) *)
end
```

Finally, apply your description to the `Unification.Unification` functor:

```ocaml
module Uni = Unification.Unification(MyTerm);;
```

You should get a `Uni` module of the following type:

```ocaml
module Uni :
  sig
    type state = Unification.Unification(MyTerm).state
    exception UseBeforeCreation
    val get_current : unit -> state
    val checkpoint : unit -> state
    val fail : state -> unit
    val cut : state -> unit
    type var = Unification.Unification(MyTerm).var
    val gen_var : unit -> var
    val union : var -> var -> bool
    val equal : var -> var -> bool
    val is_var : var -> bool
    val set_value : var -> var MyTerm.uterm -> bool
    val get_value : var -> var MyTerm.uterm option
    val get : var -> MyTerm.term option
  end
```
