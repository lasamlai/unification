open Unification
open Atomic_term
module MyTerm = Term_of_EqType (String)
module Uni = Var (MyTerm);;

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
  let v1 = Uni.gen_var () in
  let v2 = Uni.gen_var () in
  let v3 = Uni.gen_var () in
  let v4 = Uni.gen_var () in
  assert (Uni.union v1 v2);
  assert (Uni.get v1 = None);
  assert (Uni.get v2 = None);
  assert (Uni.get v3 = None);
  assert (Uni.get v4 = None);

  let v : 'a MyTerm.uterm = "Foo" in

  assert (Uni.set_value v1 v);

  assert (Uni.get v1 = Some "Foo");
  assert (Uni.get v2 = Some "Foo");
  assert (Uni.get v3 = None);
  assert (Uni.get v4 = None);

  assert (Uni.set_value v3 "Bar");

  assert (Uni.get v1 = Some "Foo");
  assert (Uni.get v2 = Some "Foo");
  assert (Uni.get v3 = Some "Bar");
  assert (Uni.get v4 = None);

  assert (Uni.union v3 v4);

  assert (Uni.get v1 = Some "Foo");
  assert (Uni.get v2 = Some "Foo");
  assert (Uni.get v3 = Some "Bar");
  assert (Uni.get v4 = Some "Bar");

  assert (not @@ Uni.union v1 v4);

  assert (Uni.get v1 = Some "Foo");
  assert (Uni.get v2 = Some "Foo");
  assert (Uni.get v3 = Some "Bar");
  assert (Uni.get v4 = Some "Bar");

  true)
;;

(* Negative unification test *)
assert (
  (* Build of term: [T1 = p] *)
  let r1 = Uni.gen_var () in

  assert (Uni.set_value r1 "p");

  (* Build of term: [T2 = q] *)
  let r2 = Uni.gen_var () in

  assert (Uni.set_value r2 "q");

  assert (not @@ Uni.equal r1 r2);

  (* Try unification: T1 = T2 *)
  assert (not @@ Uni.union r1 r2);

  assert (not @@ Uni.equal r1 r2);
  true)
;;

(* Check point test *)
assert (
  let v1 = Uni.var_of_uterm "p" in
  let v2 = Uni.gen_var () in
  let v3 = Uni.gen_var () in
  let v4 = Uni.var_of_uterm "q" in
  let v5 = Uni.var_of_uterm "p" in

  assert (not @@ Uni.equal v1 v2);
  assert (not @@ Uni.equal v1 v3);
  assert (not @@ Uni.equal v1 v4);
  assert (not @@ Uni.equal v2 v3);
  assert (not @@ Uni.equal v3 v4);

  let cc = Uni.checkpoint () in

  assert (Uni.union v2 v3);
  assert (Uni.union v1 v2);
  assert (not @@ Uni.union v3 v4);
  assert (Uni.union v3 v5);

  assert (Uni.equal v1 v2);
  assert (Uni.equal v1 v3);
  assert (not @@ Uni.equal v1 v4);
  assert (Uni.equal v1 v5);

  Uni.fail cc;

  assert (not @@ Uni.equal v1 v2);
  assert (not @@ Uni.equal v1 v3);
  assert (not @@ Uni.equal v1 v4);

  assert (Uni.is_var v2);
  assert (Uni.is_var v3);

  true)
;;

print_endline "[atomic term] OK"
