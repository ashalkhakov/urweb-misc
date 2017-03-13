(* based on Andrew Kennedy's Pickler Combinators *)

con t :: Type -> Type -> Type

val pickle : a ::: Type -> s ::: Type -> t a s -> s -> a -> string
val unpickle : a ::: Type -> s ::: Type -> t a s -> s -> string -> a

val lift : a ::: Type -> s ::: Type -> a -> t a s
val sequ : a ::: Type -> b ::: Type -> s ::: Type -> (b -> a) -> t a s -> (a -> t b s) -> t b s
val use_state : a ::: Type -> s ::: Type -> (a -> s -> s) -> (s -> t a s) -> t a s
val wrap : a ::: Type -> b ::: Type -> s ::: Type -> (a -> b) -> (b -> a) -> t a s -> t b s

(* given a record of PUs,
** apply them in the order given by the folder,
** yielding a PU for the record
*)
val record : r ::: {Type} -> s ::: Type -> $(map (fn x => t x s) r) -> folder r -> t $r s
(*
val variant : r ::: {Unit} -> a ::: Type -> s ::: Type
	      -> eq a -> pu a s -> $(map (fn x => x -> a * a -> option (pu x s)) r) -> folder r
	      -> pu (variant r) s*)
(*
constructor/destructor pairs.... (a, b) -> c vs c -> (a,b)
r ::: {Type}
a ::: Type
s ::: Type
eq a
pu a s
$(map (fn x => (x -> a) * (a -> option (pu x s))) r)
pu (variant r) s
*)
