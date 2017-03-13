(*
NOTE: taken verbatim from [meta] library!
*)
(** A record inclusion predicate *)

con incl :: K --> {K} -> {K} -> Type

val incl : K --> r1 :: {K} -> r2 :: {K} -> [r1 ~ r2] => incl r1 (r1 ++ r2)
val proj : r1 ::: {Type} -> r2 ::: {Type} -> incl r1 r2 -> $r2 -> $r1

(*
AS-20160319: function authored by myself
- take subset r1, paired up with the given record
 *)
(*val mp : K --> r1 ::: {K} -> r2 ::: {K} -> tf :: (K -> Type) -> incl r1 r2 -> folder r2 ->
         (v :: K -> tf v)
         -> incl (map tf r1) (map tf r2)
*)                                                                  (*
val proj0 :
    u ::: Type -> r1 ::: {Type} -> r2 ::: {Type} -> tf1 ::: (Type->Type) ->
    incl r1 r2
    -> folder r1
    -> $(map (fn x => u) r1)
    -> $(map tf1 r2)
    -> $(map (fn t => tf1 t * u) r1)
*)
val inv1 : K --> nm :: Name -> t :: K -> r :: {K} -> r' :: {K}
           -> [[nm] ~ r] =>
    f :: (Name -> K -> {K} -> Type)
    -> incl ([nm = t] ++ r) r'
    -> (nm :: Name -> t :: K -> r :: {K} -> [[nm] ~ r] => f nm t ([nm = t] ++ r))
    -> f nm t r'
val inv2 : K --> nm :: Name -> t :: K -> r :: {K} -> r' :: {K}
           -> [[nm] ~ r] =>
    incl ([nm = t] ++ r) r' -> incl r r'

val fold : K --> tf :: ({K} -> Type) -> r ::: {K}
           -> (nm :: Name -> v :: K -> r' :: {K}
               -> [[nm] ~ r'] => incl ([nm = v] ++ r') r -> tf r' -> tf ([nm = v] ++ r'))
           -> tf []
           -> folder r -> tf r
