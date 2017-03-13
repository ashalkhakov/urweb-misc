(* FIXME why is this not in Basis? *)
datatype cmp = Lt | Eq | Gt
val show_cmp : show cmp
val compare : t ::: Type -> ord t -> t -> t -> cmp

(*
TODO every relvar has N keys, where:
- every key is a subset of relvar heading (and the key might be empty!)
- every key tells us that no two rows in relvar may share the same value
- there can never be two duplicate keys
- currently, key = all fields of the relvar...

TODO renaming operator?
- like "rename K to K', and L to L'" such that K',L' are fresh and disjoint
- r : {Type}
- subst aka mapping : {Name}, e.g. [A=#B,C=#F]
  - 
- forall n:Name, name in mapping -> name not in r (i.e., if we build a mapping i=[B,F], then i~r)

TODO is it possible, during projection, to enumerate only field names?

*)

(*
column operations:
- drop fields
- add fresh fields
- rename fields
  - is it not the same as dropping then adding?
*)

con subst :: {Name} -> {Type} -> {Type} -> Type
(* subst: given a record of names, transforms one record into another by renaming
names=[]: r maps to r
names=[a=b], r1 maps to r2
  - r1 is of the form r1'++[a=x], s.t. r1'~[a=x]
  - r1'~[b=x]
  - r2 = r1++[b=x] <-- at runtime, this involves copying
names=s1++s2, r1 maps to r2 (*simultaneous substitution?? or iterated substitution?? what does D4 do here?*)
  - s1~s2
  - r1~s1
  - r1~s2
 *)
(*val subst : s :: {Name} -> r1 ::: {Type} -> r2 ::: {Type} -> subst s r1 r2 -> $r1 -> $r2*)

structure Subst : sig
    (* identity substitution *)
    val nil : r ::: {Type} -> subst [] r r

    (* rename n to m *)
    val cons : r ::: {Type} -> n :: Name -> m :: Name -> t :: Type
	       -> [[n]~r] => [[m]~r] => [[n]~[m]] => subst [n=m] ([n=t]++r) ([m=t]++r)
(*
    (* composition of substitutions *)
    val concat : s1 :: {Name} -> s2 :: {Name}
		 -> r1 ::: {Type} -> r2 ::: {Type} -> r3 ::: {Type}
		 -> [s1 ~ s2] =>
	subst s1 r1 r2
	-> subst s2 r2 r3
	-> subst (s1++s2) r1 r3*)
end
(*
subst s r1 r2
fun complex_rename [r1 ::: {Type}] [r2 ::: {Type}] [s ::: {Name}] [r3 ::: {Type}]
		   (pfincl : Incl.incl r1 r2) (* r1 is a subset of r2 *)
		   (pf : renaming s r1 r2) (* s is a renaming of r1 to r3 *)
		   (fl : folder (r1++r2))
		   (x : $(r1++r2)) : $(r1++r3) =
				     error <xml/>*)

con relvar :: {Type} -> Type
con order (t :: Type) = t -> t -> cmp

val empty : r :: {Type} -> folder r -> $(map ord r) -> order $r * relvar r
val insert : r ::: {Type} -> order $r -> $r -> relvar r -> relvar r
val mem : r ::: {Type} -> order $r -> $r -> relvar r -> bool
val show_rel : r ::: {Type} -> ($r -> string) -> relvar r -> string
val tablize_rel : r ::: {Type}
		  -> folder r
		  -> $(map (fn _ => string) r)
		  -> $(map show r)
		  -> relvar r -> xml [Body,Dyn,MakeForm] [] []

val project : r1 :: {Type} -> r2 ::: {Type} -> [r1 ~ r2]
    => folder r1 -> $(map ord r1) -> relvar (r1++r2) -> order $r1 * relvar r1
val restrict : r ::: {Type} -> ($r -> bool) -> relvar r -> relvar r
val union : r ::: {Type} -> order $r -> relvar r -> relvar r -> relvar r
val difference : r ::: {Type} -> order $r -> relvar r -> relvar r -> relvar r

(*natural equi-join; the subset of columns being joined on is r3*)
val join : r1 ::: {Type} -> r2 ::: {Type} -> r3 :: {Type} -> [r1 ~ r2] => [r2 ~ r3] => [r1 ~ r3]
    => (folder (r1++r2)) -> $(map ord (r1++r2)) -> folder r3 -> $(map ord r3) -> folder (r1++r2++r3)
       -> relvar (r1 ++ r3) -> relvar (r2 ++ r3)
       -> order $(r1++r2++r3) * relvar (r1++r2++r3)

(* cross-join? this is most similar to the Cartesian product *)
val times : r ::: {Type} -> s ::: {Type} -> [r ~ s]
    => folder (r++s) -> $(map ord r) -> $(map ord s) -> relvar r -> relvar s
       -> order $(r ++ s) * relvar (r++s)
