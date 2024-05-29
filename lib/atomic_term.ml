open Term

module type EqType = sig
  type t

  val equal : t -> t -> bool
end

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Term_of_EqType (Eq : EqType) :
  Term with type term = Eq.t and type 'a uterm = Eq.t = struct
  type term = Eq.t
  type 'a uterm = Eq.t

  let unwrap t = t
  let map _ t = t
  let children_of _ = []
  let build _ a = Some a
  let union _ = Eq.equal
  let equal _ = Eq.equal
end

module Term_of_OrderedType (Ord : OrderedType) :
  Term with type term = Ord.t and type 'a uterm = Ord.t = struct
  type term = Ord.t
  type 'a uterm = Ord.t

  let unwrap t = t
  let map _ t = t
  let children_of _ = []
  let build _ a = Some a
  let union _ a b = 0 = Ord.compare a b
  let equal _ a b = 0 = Ord.compare a b
end
