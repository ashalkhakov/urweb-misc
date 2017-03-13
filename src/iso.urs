con iso :: Type -> Type -> Type

val inverse : a ::: Type -> b ::: Type -> iso a b -> iso b a
val apply : a ::: Type -> b ::: Type -> iso a b -> a -> option b
val unapply : a ::: Type -> b ::: Type -> iso a b -> b -> option a

val category_iso : Categorial.category iso
(*
val prod : r ::: {Type*Type} -> folder r -> $(map (fn x => iso (fst x) (snd x)) r) -> iso $(map fst r) $(map snd r)
val sum : r ::: {Type} -> a ::: Type -> folder r -> $(map (fn x => iso x a) r) -> iso (variant r) a
*)
(* here, derivation for recursive types *)
