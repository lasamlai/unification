open Story

module Backtracking_references : sig
  type state

  exception UseBeforeCreation

  val get_current : unit -> state
  val checkpoint : unit -> state
  val fail : state -> unit
  val cut : state -> unit

  type 'a href

  val href : 'a -> 'a href
  val ( ! ) : 'a href -> 'a
  val ( := ) : 'a href -> 'a -> unit
end = struct
  include Story ()

  exception UseBeforeCreation

  type 'a href = {
    mutable contents : 'a;
    mutable current : state;
    mutable history : ('a * state) list;
  }

  let actualize (hr : 'a href) =
    let rec actualize story (value : 'a option) =
      match story with
      | Protect state ->
          (match value with
          | None -> (
              match hr.history with
              | (con, cur) :: history ->
                  hr.current <- cur;
                  hr.contents <- con;
                  hr.history <- history
              | _ -> raise UseBeforeCreation)
          | Some v -> hr.contents <- v);
          hr.history <- (hr.contents, hr.current) :: hr.history;
          hr.current <- state
      | NoChange -> (
          match value with
          | None -> (
              match hr.history with
              | (con, cur) :: history ->
                  hr.current <- cur;
                  hr.contents <- con;
                  hr.history <- history
              | _ -> raise UseBeforeCreation)
          | Some v -> hr.contents <- v)
      | Fail (st, story) -> (
          match hr.history with
          | (con, cur) :: history ->
              if cur == st then (
                hr.current <- cur;
                hr.contents <- con;
                hr.history <- history;
                actualize story (Some con))
              else (
                hr.current <- st;
                actualize story None)
          | _ -> raise UseBeforeCreation)
      | Cut (st, story) ->
          (match hr.history with
          | (_, cur) :: history -> if cur == st then hr.history <- history
          | _ -> raise UseBeforeCreation);
          hr.current <- st;
          actualize story value
    in
    actualize (story hr.current) (Some hr.contents)

  let href x = { contents = x; current = get_current (); history = [] }

  let get hr =
    actualize hr;
    hr.contents

  let set (hr : 'a href) (a : 'a) =
    actualize hr;
    hr.contents <- a

  let ( ! ) = get
  let ( := ) = set
end
