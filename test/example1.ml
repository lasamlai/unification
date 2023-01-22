open Unification

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

  (** `equal eq t1 t2` is `true` if terms are equal, `false` otherwise.
   *  `eq x1 x2` is `true` if the variables are equal, `false` otherwise.
   *
   *  Note: Two variables are equal if they contain equal terms or have been unified.
   *  This should work like `==/2` in the Prolog.
   *
   *  Warning: The information that two terms are equal will be cached in the `Unification` structure. *)
  let equal equal ta tb =
    match (ta, tb) with
    | UVar sa, UVar sb -> sa = sb
    | UImpl (ta1, ta2), UImpl (tb1, tb2) -> equal ta1 tb1 && equal ta2 tb2
    | UBox ta, UBox tb -> equal ta tb
    | _ -> false
end

module Uni = Var (MyTerm)
open MyTerm;;

assert (
  let open Uni in
  let v1 = Uni.gen_var () in
  let v2 = Uni.gen_var () in
  let v3 = Uni.gen_var () in
  let v4 = Uni.gen_var () in
  let c = checkpoint () in
  assert (Uni.equal v1 v2 = false);
  assert (Uni.equal v1 v3 = false);
  assert (Uni.equal v1 v4 = false);
  assert (Uni.equal v2 v3 = false);
  assert (Uni.equal v2 v4 = false);
  assert (Uni.equal v3 v4 = false);

  assert (Uni.union v1 v2);
  assert (Uni.equal v1 v2 = true);
  assert (Uni.equal v1 v3 = false);
  assert (Uni.equal v1 v4 = false);
  assert (Uni.equal v2 v3 = false);
  assert (Uni.equal v2 v4 = false);
  assert (Uni.equal v3 v4 = false);

  fail c;
  assert (Uni.equal v1 v2 = false);
  assert (Uni.equal v1 v3 = false);
  assert (Uni.equal v1 v4 = false);
  assert (Uni.equal v2 v3 = false);
  assert (Uni.equal v2 v4 = false);
  assert (Uni.equal v3 v4 = false);

  assert (Uni.union v1 v2);
  assert (Uni.equal v1 v2 = true);
  assert (Uni.equal v1 v3 = false);
  assert (Uni.equal v1 v4 = false);
  assert (Uni.equal v2 v3 = false);
  assert (Uni.equal v2 v4 = false);
  assert (Uni.equal v3 v4 = false);

  assert (Uni.union v3 v4);
  assert (Uni.equal v1 v2 = true);
  assert (Uni.equal v1 v3 = false);
  assert (Uni.equal v1 v4 = false);
  assert (Uni.equal v2 v3 = false);
  assert (Uni.equal v2 v4 = false);
  assert (Uni.equal v3 v4 = true);

  let c = checkpoint () in

  assert (Uni.union v1 v3);
  assert (Uni.equal v1 v2 = true);
  assert (Uni.equal v1 v3 = true);
  assert (Uni.equal v1 v4 = true);
  assert (Uni.equal v2 v3 = true);
  assert (Uni.equal v2 v4 = true);
  assert (Uni.equal v3 v4 = true);

  fail c;
  assert (Uni.equal v1 v2 = true);
  assert (Uni.equal v1 v3 = false);
  assert (Uni.equal v1 v4 = false);
  assert (Uni.equal v2 v3 = false);
  assert (Uni.equal v2 v4 = false);
  assert (Uni.equal v3 v4 = true);

  true)
;;

(* Test of union find *)
assert (
  let v1 = Uni.gen_var () in
  let v2 = Uni.gen_var () in
  let v3 = Uni.gen_var () in
  let v4 = Uni.gen_var () in
  assert (Uni.equal v1 v2 = false);
  assert (Uni.equal v1 v3 = false);
  assert (Uni.equal v1 v4 = false);
  assert (Uni.equal v2 v3 = false);
  assert (Uni.equal v2 v4 = false);
  assert (Uni.equal v3 v4 = false);
  assert (Uni.union v1 v2);
  assert (Uni.equal v1 v2 = true);
  assert (Uni.equal v1 v3 = false);
  assert (Uni.equal v1 v4 = false);
  assert (Uni.equal v2 v3 = false);
  assert (Uni.equal v2 v4 = false);
  assert (Uni.equal v3 v4 = false);
  assert (Uni.union v3 v4);
  assert (Uni.equal v1 v2 = true);
  assert (Uni.equal v1 v3 = false);
  assert (Uni.equal v1 v4 = false);
  assert (Uni.equal v2 v3 = false);
  assert (Uni.equal v2 v4 = false);
  assert (Uni.equal v3 v4 = true);
  assert (Uni.union v1 v3);
  assert (Uni.equal v1 v2 = true);
  assert (Uni.equal v1 v3 = true);
  assert (Uni.equal v1 v4 = true);
  assert (Uni.equal v2 v3 = true);
  assert (Uni.equal v2 v4 = true);
  assert (Uni.equal v3 v4 = true);
  true)
;;

(* Test Union-Find with values *)
assert (
  let open MyTerm in
  let v1 = Uni.gen_var () in
  let v2 = Uni.gen_var () in
  let v3 = Uni.gen_var () in
  let v4 = Uni.gen_var () in
  assert (Uni.union v1 v2);
  assert (Uni.get v1 = None);
  assert (Uni.get v2 = None);
  assert (Uni.get v3 = None);
  assert (Uni.get v4 = None);

  assert (Uni.set_value v1 (UVar "Foo"));

  assert (Uni.get v1 = Some (Var "Foo"));
  assert (Uni.get v2 = Some (Var "Foo"));
  assert (Uni.get v3 = None);
  assert (Uni.get v4 = None);

  assert (Uni.set_value v3 (UVar "Bar"));

  assert (Uni.get v1 = Some (Var "Foo"));
  assert (Uni.get v2 = Some (Var "Foo"));
  assert (Uni.get v3 = Some (Var "Bar"));
  assert (Uni.get v4 = None);

  assert (Uni.union v3 v4);

  assert (Uni.get v1 = Some (Var "Foo"));
  assert (Uni.get v2 = Some (Var "Foo"));
  assert (Uni.get v3 = Some (Var "Bar"));
  assert (Uni.get v4 = Some (Var "Bar"));

  assert (not @@ Uni.union v1 v4);

  assert (Uni.get v1 = Some (Var "Foo"));
  assert (Uni.get v2 = Some (Var "Foo"));
  assert (Uni.get v3 = Some (Var "Bar"));
  assert (Uni.get v4 = Some (Var "Bar"));

  true)
;;

(* Test of building terms *)

assert (
  let v1 = Uni.gen_var () in
  let v2 = Uni.gen_var () in
  let v3 = Uni.gen_var () in
  let v4 = Uni.gen_var () in

  assert (Uni.union v1 v2);

  assert (Uni.set_value v1 (UImpl (v3, v4)));

  assert (Uni.get v1 = None);
  assert (Uni.get v2 = None);
  assert (Uni.get v3 = None);
  assert (Uni.get v4 = None);

  assert (Uni.set_value v3 (UVar "p"));

  assert (Uni.get v1 = None);
  assert (Uni.get v2 = None);
  assert (Uni.get v3 = Some (Var "p"));
  assert (Uni.get v4 = None);

  assert (Uni.set_value v4 (UVar "q"));

  assert (Uni.get v1 = Some (Impl (Var "p", Var "q")));
  assert (Uni.get v2 = Some (Impl (Var "p", Var "q")));
  assert (Uni.get v3 = Some (Var "p"));
  assert (Uni.get v4 = Some (Var "q"));
  true)
;;

(* Equal of term *)
assert (
  let v1 = Uni.gen_var () in
  let v2 = Uni.gen_var () in
  assert (Uni.set_value v1 (UVar "p"));
  assert (Uni.set_value v2 (UVar "p"));
  Uni.equal v1 v2)
;;

(* Unification test *)
assert (
  (* Build of term: T1 = imp(L1, p) *)
  let t1 = Uni.gen_var () in
  let l1 = Uni.gen_var () in
  let r1 = Uni.gen_var () in

  assert (Uni.set_value t1 (UImpl (l1, r1)));
  assert (Uni.set_value r1 (UVar "p"));

  (* Build of term: T2 = imp(L2, p) *)
  let t2 = Uni.gen_var () in
  let l2 = Uni.gen_var () in
  let r2 = Uni.gen_var () in

  assert (Uni.set_value t2 (UImpl (l2, r2)));
  assert (Uni.set_value r2 (UVar "p"));

  assert (not @@ Uni.equal l1 l2);
  assert (Uni.equal r1 r2);

  (* Try unification: T1 = T2 *)
  assert (Uni.union t1 t2);

  assert (Uni.equal t1 t2);
  assert (Uni.equal r1 r2);
  assert (Uni.equal l1 l2);
  true)
;;

(* Negative unification test *)
assert (
  (* Build of term: T1 = imp(L1, p) *)
  let t1 = Uni.gen_var () in
  let l1 = Uni.gen_var () in
  let r1 = Uni.gen_var () in

  assert (Uni.set_value t1 (UImpl (l1, r1)));
  assert (Uni.set_value r1 (UVar "p"));

  (* Build of term: T2 = imp(L2, q) *)
  let t2 = Uni.gen_var () in
  let l2 = Uni.gen_var () in
  let r2 = Uni.gen_var () in

  assert (Uni.set_value t2 (UImpl (l2, r2)));
  assert (Uni.set_value r2 (UVar "q"));

  assert (not @@ Uni.equal l1 l2);

  (* Try unification: T1 = T2 *)
  assert (not @@ Uni.union t1 t2);

  assert (not @@ Uni.equal t1 t2);
  assert (not @@ Uni.equal l1 l2);
  true)
;;

(* Check point test *)
assert (
  (* Build of term: T1 = imp(L1, p) *)
  let t1 = Uni.gen_var () in
  let l1 = Uni.gen_var () in
  let r1 = Uni.gen_var () in

  assert (Uni.set_value t1 (UImpl (l1, r1)));
  assert (Uni.set_value r1 (UVar "p"));

  (* Build of term: T2 = imp(L2, q) *)
  let t2 = Uni.gen_var () in
  let l2 = Uni.gen_var () in
  let r2 = Uni.gen_var () in

  assert (Uni.set_value t2 (UImpl (l2, r2)));
  assert (Uni.set_value r2 (UVar "p"));

  assert (not @@ Uni.equal t1 t2);
  assert (not @@ Uni.equal l1 l2);
  assert (Uni.equal r1 r2);

  let cc = Uni.checkpoint () in

  assert (Uni.union t1 t2);

  assert (Uni.equal t1 t2);
  assert (Uni.equal l1 l2);
  assert (Uni.equal r1 r2);

  Uni.fail cc;

  assert (not @@ Uni.equal t1 t2);
  assert (not @@ Uni.equal l1 l2);
  assert (Uni.equal r1 r2);

  true)
;;

(* Unallow recursive terms *)
assert (
  (* Build of term: T1 = box(t2) *)
  let t1 = Uni.gen_var () in
  let t2 = Uni.gen_var () in

  assert (Uni.set_value t1 (UBox t2));

  assert (not @@ Uni.equal t1 t2);
  assert (not @@ Uni.union t1 t2);
  assert (not @@ Uni.equal t1 t2);
  true)
;;

(* Unallow recursive terms in `set_value` *)
assert (
  let x = Uni.gen_var () in
  assert (not @@ Uni.set_value x (UBox x));

  Uni.get x = None)
;;

print_endline "[example 1] OK"
