open Unification

type var = string
type ty = Var of var | Impl of ty * ty

module Ty = struct
  type term = ty
  type 'a uterm = Var of var | Impl of 'a * 'a

  let unwrap (t : term) : term uterm =
    match t with Var v -> Var v | Impl (t1, t2) -> Impl (t1, t2)

  let map f t =
    match t with Var v -> Var v | Impl (v1, v2) -> Impl (f v1, f v2)

  let children_of (t : 'a uterm) : 'a list =
    match t with Var _ -> [] | Impl (t1, t2) -> [ t1; t2 ]

  let build get (t : 'a uterm) : term option =
    match t with
    | Var s -> Some (Var s)
    | Impl (t1, t2) -> (
        match (get t1, get t2) with
        | Some t1, Some t2 -> Some (Impl (t1, t2))
        | _ -> None)

  let union union (ta : 'a uterm) (tb : 'a uterm) =
    match (ta, tb) with
    | Var sa, Var sb -> sa = sb
    | Impl (ta1, ta2), Impl (tb1, tb2) -> union ta1 tb1 && union ta2 tb2
    | _ -> false

  let equal equal ta tb =
    match (ta, tb) with
    | Var sa, Var sb -> sa = sb
    | Impl (ta1, ta2), Impl (tb1, tb2) -> equal ta1 tb1 && equal ta2 tb2
    | _ -> false
end

module VarTy = Var (Ty)

type lam = Var of var | Lam of var * ty * lam | App of lam * lam

module Lam = struct
  type term = lam
  type 'a uterm = Var of var | Lam of var * VarTy.var * 'a | App of 'a * 'a

  let unwrap (t : term) =
    match t with
    | Var x -> Var x
    | Lam (x, ty, t1) -> Lam (x, VarTy.var_of_term ty, t1)
    | App (t1, t2) -> App (t1, t2)

  let map f t =
    match t with
    | Var x -> Var x
    | Lam (x, ty, v) -> Lam (x, ty, f v)
    | App (v1, v2) -> App (f v1, f v2)

  let children_of (t : 'a uterm) : 'a list =
    match t with
    | Var _ -> []
    | Lam (_, _, t2) -> [ t2 ]
    | App (t1, t2) -> [ t1; t2 ]

  let build get (t : 'a uterm) : term option =
    match t with
    | Var s -> Some (Var s)
    | Lam (x, ty, t12) -> (
        match (VarTy.get ty, get t12) with
        | Some ty, Some t12 -> Some (Lam (x, ty, t12))
        | _ -> None)
    | App (t1, t2) -> (
        match (get t1, get t2) with
        | Some t1, Some t2 -> Some (App (t1, t2))
        | _ -> None)

  let union union (ta : 'a uterm) (tb : 'a uterm) =
    match (ta, tb) with
    | Var sa, Var sb -> sa = sb
    | Lam (x1, ty1, t1), Lam (x2, ty2, t2) when x1 = x2 ->
        VarTy.union ty1 ty2 && union t1 t2
    | App (ta1, ta2), App (tb1, tb2) -> union ta1 tb1 && union ta2 tb2
    | _ -> false

  let equal equal ta tb =
    match (ta, tb) with
    | Var sa, Var sb -> sa = sb
    | Lam (x1, ty1, t1), Lam (x2, ty2, t2) when x1 = x2 ->
        VarTy.equal ty1 ty2 && equal t1 t2
    | App (ta1, ta2), App (tb1, tb2) -> equal ta1 tb1 && equal ta2 tb2
    | _ -> false
end

module VarLam = Var (Lam)
open Ty;;

assert (
  let open VarTy in
  let x = gen_var () in
  let y = gen_var () in
  let z = gen_var () in

  assert (set_value x (Var "p"));
  assert (set_value y (Var "q"));
  assert (set_value z (Impl (x, y)));

  let t : ty = Impl (Var "p", Var "q") in

  get z = Some t)
;;

assert (
  let open VarLam in
  let x = gen_var () in
  let y : VarTy.var = VarTy.gen_var () in
  let z = gen_var () in

  assert (set_value x (Var "x"));
  assert (VarTy.set_value y (Var "p"));
  assert (set_value z (Lam ("x", y, x)));

  let (t : lam) = Lam ("x", Var "p", Var "x") in

  get z = Some t)
;;

assert (
  let open VarLam in
  let a : VarTy.var = VarTy.gen_var () in
  let b : VarTy.var = VarTy.gen_var () in

  assert (VarTy.set_value a (Var "a"));
  assert (VarTy.set_value b (Var "b"));

  let f = gen_var () in
  let x = gen_var () in
  assert (set_value x (Var "x"));
  assert (set_value f (Var "f"));

  let app = gen_var () in
  let app2 = gen_var () in
  let lf = gen_var () in
  let n = gen_var () in
  assert (set_value n (Lam ("x", a, lf)));
  assert (set_value lf (Lam ("f", b, app)));
  assert (set_value app (App (f, app2)));
  assert (set_value app2 (App (f, x)));

  let (t : lam) =
    Lam ("x", Var "a", Lam ("f", Var "b", App (Var "f", App (Var "f", Var "x"))))
  in

  get n = Some t)
;;

print_endline "[example 2] OK"
