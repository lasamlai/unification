open Story;;

assert (
  let open Story (struct end) in
  let c1 = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  let[@warning "-8"] (Protect x) = story c1 in
  x == get_current ())
;;

assert (
  let open Story (struct end) in
  let c1 = checkpoint () in
  let _ = checkpoint () in
  let c3 = checkpoint () in
  let c4 = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  cut c3;
  let[@warning "-8"] (Protect x) = story c1 in
  assert (x == c3);
  let[@warning "-8"] (Cut (x, NoChange)) = story c4 in
  x == c3)
;;

assert (
  let open Story (struct end) in
  let cur = get_current () in
  NoChange = story cur)
;;

assert (
  let open Story (struct end) in
  let cur = get_current () in
  let c = checkpoint () in
  cur == c)
;;

assert (
  let open Story (struct end) in
  let rec last st =
    match[@warning "-8"] st with
    | Fail (_, NoChange) | Cut (_, NoChange) -> st
    | Fail (_, st) | Cut (_, st) -> last st
  in
  let c1 = checkpoint () in
  let c2 = checkpoint () in
  let c3 = checkpoint () in
  let c4 = checkpoint () in
  let c5 = checkpoint () in
  fail c4;
  let[@warning "-8"] (Fail (x, NoChange)) = story c5 in
  assert (x == c4);
  fail c3;
  let[@warning "-8"] (Fail (x, NoChange)) = story c4 in
  assert (x == c3);
  fail c2;
  let[@warning "-8"] (Fail (x, NoChange)) = story c3 in
  assert (x == c2);
  fail c1;
  let[@warning "-8"] (Fail (x, NoChange)) = story c2 in
  assert (x == c1);
  let[@warning "-8"] (Fail (x, NoChange)) = last (story c5) in
  assert (x == c1);
  let[@warning "-8"] (Fail (x, NoChange)) = last (story c4) in
  assert (x == c1);
  let[@warning "-8"] (Fail (x, NoChange)) = last (story c3) in
  assert (x == c1);
  let[@warning "-8"] (Fail (x, NoChange)) = last (story c2) in
  assert (x == c1);
  let[@warning "-8"] NoChange = story c1 in
  true)
;;

assert (
  let open Story (struct end) in
  let rec last st =
    match[@warning "-8"] st with
    | Fail (_, NoChange) | Cut (_, NoChange) -> st
    | Fail (_, st) | Cut (_, st) -> last st
  in
  let c1 = checkpoint () in
  let c2 = checkpoint () in
  let c3 = checkpoint () in
  let c4 = checkpoint () in
  let c5 = checkpoint () in
  cut c4;
  let[@warning "-8"] (Cut (x, NoChange)) = story c5 in
  assert (x == c4);
  cut c3;
  let[@warning "-8"] (Cut (x, NoChange)) = story c4 in
  assert (x == c3);
  cut c2;
  let[@warning "-8"] (Cut (x, NoChange)) = story c3 in
  assert (x == c2);
  cut c1;
  let[@warning "-8"] (Cut (x, NoChange)) = story c2 in
  assert (x == c1);
  let[@warning "-8"] (Cut (x, NoChange)) = last (story c5) in
  assert (x == c1);
  let[@warning "-8"] (Cut (x, NoChange)) = last (story c4) in
  assert (x == c1);
  let[@warning "-8"] (Cut (x, NoChange)) = last (story c3) in
  assert (x == c1);
  let[@warning "-8"] (Cut (x, NoChange)) = last (story c2) in
  assert (x == c1);
  let[@warning "-8"] NoChange = story c1 in
  true)
;;

assert (
  let open Story (struct end) in
  let c1 = checkpoint () in
  let c2 = checkpoint () in
  let c3 = checkpoint () in
  let c4 = checkpoint () in
  let c5 = checkpoint () in
  cut c4;
  fail c3;
  cut c2;
  fail c1;
  let[@warning "-8"] (Cut (v4, Fail (v3, Cut (v2, Fail (v1, NoChange))))) =
    story c5
  in
  assert (v4 == c4);
  assert (v3 == c3);
  assert (v2 == c2);
  assert (v1 == c1);
  let[@warning "-8"] (Fail (v3, Cut (v2, Fail (v1, NoChange)))) = story c4 in
  assert (v3 == c3);
  assert (v2 == c2);
  assert (v1 == c1);
  let[@warning "-8"] (Cut (v2, Fail (v1, NoChange))) = story c3 in
  assert (v2 == c2);
  assert (v1 == c1);
  let[@warning "-8"] (Fail (v1, NoChange)) = story c2 in
  assert (v1 == c1);
  let[@warning "-8"] NoChange = story c1 in
  true)
;;

assert (
  let open Story (struct end) in
  let c1 = checkpoint () in
  let c2 = checkpoint () in
  let c3 = checkpoint () in
  let c4 = checkpoint () in
  let c5 = checkpoint () in
  cut c4;
  fail c3;
  cut c2;
  fail c1;
  let c6 = checkpoint () in
  assert (c1 == c6);
  let c7 = checkpoint () in
  let cur = get_current () in
  let[@warning "-8"] (Cut (v4, Fail (v3, Cut (v2, Fail (v1, Protect vc))))) =
    story c5
  in
  assert (v4 == c4);
  assert (v3 == c3);
  assert (v2 == c2);
  assert (v1 == c1);
  assert (vc == cur);
  let[@warning "-8"] (Fail (v3, Cut (v2, Fail (v1, Protect vc)))) = story c4 in
  assert (v3 == c3);
  assert (v2 == c2);
  assert (v1 == c1);
  assert (vc == cur);
  let[@warning "-8"] (Cut (v2, Fail (v1, Protect vc))) = story c3 in
  assert (v2 == c2);
  assert (v1 == c1);
  assert (vc == cur);
  let[@warning "-8"] (Fail (v1, Protect vc)) = story c2 in
  assert (v1 == c1);
  assert (vc == cur);
  let[@warning "-8"] (Protect vc) = story c1 in
  assert (vc == cur);
  let[@warning "-8"] (Protect vc) = story c6 in
  assert (vc == cur);
  let[@warning "-8"] (Protect vc) = story c7 in
  assert (vc == cur);
  true)
;;

assert (
  let open Story (struct end) in
  let start = get_current () in

  let c1 = checkpoint () in
  assert (start == c1);
  let cur = get_current () in
  assert (NoChange = story cur);
  let[@warning "-8"] (Protect nc) = story start in
  assert (nc == cur);
  fail c1;
  let[@warning "-8"] (Fail (vc, NoChange)) = story cur in
  assert (vc == start);

  let c2 = checkpoint () in
  assert (start == c2);
  let cur = get_current () in
  assert (NoChange = story cur);
  let[@warning "-8"] (Protect nc) = story start in
  assert (nc == cur);
  fail c2;
  let[@warning "-8"] (Fail (vc, NoChange)) = story cur in
  assert (vc == start);

  let c3 = checkpoint () in
  assert (start == c3);
  let cur = get_current () in
  assert (NoChange = story cur);
  let[@warning "-8"] (Protect nc) = story start in
  assert (nc == cur);
  cut c3;
  let[@warning "-8"] (Cut (vc, NoChange)) = story cur in
  assert (vc == start);

  let c4 = checkpoint () in
  assert (start == c4);
  let cur = get_current () in
  assert (NoChange = story cur);
  let[@warning "-8"] (Protect nc) = story start in
  assert (nc == cur);
  cut c4;
  let[@warning "-8"] (Cut (vc, NoChange)) = story cur in
  assert (vc == start);

  let c5 = checkpoint () in
  assert (start == c5);
  let cur = get_current () in
  assert (NoChange = story cur);
  let[@warning "-8"] (Protect nc) = story start in
  assert (nc == cur);
  fail c5;
  let[@warning "-8"] (Fail (vc, NoChange)) = story cur in
  assert (vc == start);

  assert (NoChange = story start);

  true)
;;

print_endline "[story] OK"
