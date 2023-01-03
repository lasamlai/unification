module Story (Unit : sig end) : sig
  type state

  val get_current : unit -> state
  val checkpoint : unit -> state
  val fail : state -> unit
  val cut : state -> unit

  type story =
    | Fail of state * story
    | Cut of state * story
    | Protect of state
    | NoChange

  val story : state -> story

  type next = NFail of state | NCut of state | NProtect of state | NNoChange

  val next : state -> next
end = struct
  type next_state =
    | Current
    | Update of state
    | Rollback of state
    | Commit of state

  and state = next_state ref

  let begining : state = ref Current
  let current = ref begining
  let get_current () = !current

  let is_valid_checkpoint chp =
    let rec aux x =
      x == chp || match !x with Update x -> aux x | _ -> assert false
    in
    aux begining

  let checkpoint () : state =
    let acc = !current in
    assert (!acc = Current);
    let new_point = ref Current in
    acc := Update new_point;
    current := new_point;
    acc

  let fail chp =
    assert (is_valid_checkpoint chp);
    let rec aux ok ptr =
      match !ptr with
      | Current -> ptr := Rollback ok
      | Update x ->
          ptr := Rollback ok;
          aux ptr x
      | _ -> failwith "Error: Story line is broken!"
    in
    aux chp chp;
    chp := Current;
    current := chp

  let cut chp =
    assert (is_valid_checkpoint chp);
    let rec aux ok ptr =
      match !ptr with
      | Current -> ptr := Commit ok
      | Update x ->
          ptr := Commit ok;
          aux ptr x
      | _ -> failwith "Error: Story line is broken!"
    in
    aux chp chp;
    chp := Current;
    current := chp

  type story =
    | Fail of state * story
    | Cut of state * story
    | Protect of state
    | NoChange

  (* Deprecated *)
  let story chp : story =
    (* TODO: Zwijanie niepotrzebnej hstorii *)
    let rec story chp updating : story =
      match !chp with
      | Current -> if updating then Protect chp else NoChange
      | Update chp -> story chp true
      | Rollback chp -> Fail (chp, story chp false)
      | Commit chp -> Cut (chp, story chp false)
    in
    story chp false

  type next = NFail of state | NCut of state | NProtect of state | NNoChange

  let next chp : next =
    let rec last x =
      match !chp with
      | Current -> x
      | Update x -> last x
      | _ -> failwith "Error: Story line is broken!"
    in
    match !chp with
    | Current -> NNoChange
    | Update chp -> NProtect (last chp)
    | Rollback chp -> NFail chp
    | Commit chp -> NCut chp
end
