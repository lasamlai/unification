open Backtracking_references
open Backtracking_references;;

assert (
  let x = href 10 in
  let _ = checkpoint () in
  let c = checkpoint () in
  let _ = checkpoint () in
  x := 20;
  fail c;
  !x = 10)
;;

assert (
  let x = href 10 in
  let y = href 12 in
  let get = ( ! ) in
  let set = ( := ) in
  assert (get x = 10);
  assert (get y = 12);
  let c = checkpoint () in
  assert (get x = 10);
  assert (get y = 12);
  set x 20;
  set y 22;
  assert (get x = 20);
  assert (get y = 22);
  fail c;
  assert (get x = 10);
  assert (get y = 12);
  set x 0;
  set y 2;
  assert (get x = 0);
  assert (get y = 2);

  let c = checkpoint () in
  assert (get x = 0);
  assert (get y = 2);
  set x 30;
  set y 32;
  assert (get x = 30);
  assert (get y = 32);
  cut c;
  assert (get x = 30);
  assert (get y = 32);
  true)
;;

assert (
  let get = ( ! ) in
  let set = ( := ) in
  let x = href 10 in
  let c = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  set x 20;
  assert (get x = 20);
  fail c;
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  cut c;

  let _ = checkpoint () in

  get x = 10)
;;

assert (
  let get = ( ! ) in
  let set = ( := ) in
  let x = href 10 in
  let c = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  set x 20;
  assert (get x = 20);
  cut c;
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  let _ = checkpoint () in
  fail c;
  let _ = checkpoint () in
  get x = 20)
;;

assert (
  let get = ( ! ) in
  let c = checkpoint () in
  let x = href 10 in
  fail c;
  try
    let _ = get x in
    false
  with UseBeforeCreation -> true)
;;

assert (
  let get = ( ! ) in
  let c = checkpoint () in
  let x = href 10 in
  cut c;
  try
    let _ = get x in
    false
  with UseBeforeCreation -> true)
;;

print_endline "[href] OK"
