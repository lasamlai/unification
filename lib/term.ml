module type Term = sig
  type term
  (** Term description without unifying-variables. *)

  type 'a uterm
  (** Description of terms with unifying variables. *)

  val unwrap : term -> term uterm
  (** [unwrap t] is the first layer of term [t] with subterms [t] substituted. *)

  val map : ('a -> 'b) -> 'a uterm -> 'b uterm
  (** [map f ut] map ['a uterm] to a ['b uterm] using [f]. *)

  val children_of : 'a uterm -> 'a list
  (** [children_of t] is a list of children of term [t]. *)

  val build : ('a -> term option) -> 'a uterm -> term option
  (** [build f ut] is [Some t] if [ut] is a grounded term and [None] otherwise.
      [t] is grounded version of term [ut].
      [f x] is the term assigned to the variable [x] if [x] is grounded. *)

  val union : ('a -> 'a -> bool) -> 'a uterm -> 'a uterm -> bool
  (** [union u t1 t2] tries to unify the terms [t1] and [t2] and returns [true] if successful, [false] otherwise.
      [u x1 x2] tries to unify the variables [x1] and [x2] and returns [true] if successful, [false] otherwise. *)

  val equal : ('a -> 'a -> bool) -> 'a uterm -> 'a uterm -> bool
  (** [equal eq t1 t2] is [true] if terms are equal, [false] otherwise.
      [eq x1 x2] is [true] if the variables are equal, [false] otherwise.

      {b Note}: Two variables are equal if they contain equal terms or have been unified.
      This should work like [==/2] in the Prolog.

      {b Warning}: The information that two terms are equal will be cached in the {!module:Unification} structure. *)
end
