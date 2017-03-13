type iso a b = {F : a -> option b, G : b -> option a}

fun inverse [a] [b] i = {F = i.G, G = i.F}
fun apply [a] [b] (i : iso a b) (x : a) : option b = i.F x
fun unapply [a] [b] (i : iso a b) (x : b) : option a = i.G x

val category_iso = Categorial.mkCategory {
  Id = fn [t] => {F = fn (x : t) => Some x, G = fn (x : t) => Some x},
  Comp = fn [a] [b] [c] (g : iso b c) (f : iso a b) => {
    F = Categorial.kleisli f.F g.F,
    G = Categorial.kleisli g.G f.G
  }
}

fun times1 [t ::: {(Type * Type)}] (x : $(map (fn x => iso (fst x) (snd x)) t)) :
  iso $(map fst t) $(map snd t) = error <xml>something went wrong</xml>

fun times [a ::: Type] [b ::: Type] [g ::: Type] [d ::: Type]
	  (* (i : $(map (fn t => iso (fst t) (snd t)) r)) : iso $(map fst r) $(map snd r) *)
	  (i : iso a b) (j : iso g d) : iso (a * g) (b * d) = {
    F = fn (x : a, y : g) => (* x : $(map fst r) *)
 	   bind (apply i x) (* iterate over r? *)
		(fn (x : b) =>
		    bind (apply j y) (fn (y : d) =>
					 return (x, y))),
    G = fn (x : b, y : d) =>
	   bind (unapply i x)
		(fn (x : a) =>
		    bind (unapply j y)
			 (fn (y : g) =>
			     return (x, y)))
}

fun iso_or [a ::: Type] [b ::: Type] [g ::: Type]
	   (i : iso a g) (j : iso b g)
    : iso (variant [Left = a, Right = b]) g =
      {F = fn (v : variant [Left = a, Right = b]) =>
	      match v {Left = fn a => apply i a, Right = fn b => apply j b},
       G = fn (x : g) =>
	      case (unapply i x) of None => Option.mp (make [#Right]) (unapply j x)
				  | Some x => Some (make [#Left] x)
      }

fun iterate [a ::: Type] (step: iso a a) : iso a a = let
  fun driver (step : a -> option a) (state : a) : a =
    case step state of
    | Some state' => driver step state'
    | None => state
in
  {F = fn (x : a) => Some (driver (apply step) x),
   G = fn (x : a) => Some (driver (unapply step) x)}
end

(*
fun match1 : ts ::: {Type} -> t ::: Type -> variant ts -> $(map (fn t' => t' -> t) ts) -> t
*)
(*
fun sum1 [r ::: {Type}] [t ::: Type] (fl : folder r)
	(i : $(map (fn x => iso x t) r))
	(v : variant r) : option t =
// crush v, 
*)
(*
con variant :: {Type} -> Type
val make : nm :: Name -> t ::: Type -> ts ::: {Type} -> [[nm] ~ ts] => t -> variant ([nm = t] ++ ts)
val match : ts ::: {Type} -> t ::: Type -> variant ts -> $(map (fn t' => t' -> t) ts) -> t
*)

(* iso : Type -> Type -> Type
to : {Type} -> from : {Type}
$(map iso ())

val prod : r ::: {Type*Type} -> folder r -> $(map (fn x => iso (fst x) (snd x)) r) -> iso $(map fst r) $(map snd r)

 *)

(*
  {F = fn (v :: variant r) => match v (mp apply i)
	  (*for each case n=v, do: apply i.n v*)
   G = fn (v :: a) => (*find the first iso from i that returns Some, return it*) }

con pair = Type * Type
fun join [ts ::: {pair}] (fl : folder (map fst ts)) (r : $(map fst ts)) : option $(map snd ts) =
  @fold [fn ts => $(map fst ts) -> option $(map snd ts)]
    (fn [nm :: _] [v :: _] [r :: _] [[nm] ~ r] (f : $(map fst r) -> option $(map snd r)) =>
       fn r : $(map fst ([nm = v] ++ r)) =>
          case f (r -- nm) of
          | None => None
          | Some vs => Some ({nm = v} ++ vs))
    (fn _ : $(map option []) => Some {}) fl r*)
(*

fun prod [r ::: {Type * Type}] (fl : folder r) (i :: $(map (fn x => iso (fst x) (snd x)) r) = {
  F = (* fn (a, ..., b) => liftM (fun x ... y -> (x, ..., y)) (apply i.0 a) ... (apply i.n b) *)
(* F : $(map fst r) -> option $(map snd r)
given rs : $(map fst r),
initial: start with Some {}
step: given [n=v] in rs, case (apply i.n v) of None -> None | Some v -> Some {xs ++ [n = v]}
  where xs is the intermediate result type, option $(map snd r)
*)
      fn (rs ::: $(map fst r)) =>
         (* foreach [n=v] in rs, let v' = apply i.[#n] v in [n=v'] end
however, if v' is None, then we must simply return None (i.e., monadic sequencing here) *)
        @fold [fn r => $(map fst r) -> option (
        @fold [fn r => iso $(map fst r) $(map snd r)]
              (fn [nm :: _] [v :: _] [r :: _] [[nm] ~ r] x xs =>
                xs ++ {#nm = apply i.[#nm] x}
              ) {} fl i,

  G = (* fn (a, ..., b) => liftM (fun x ... y -> (x, ..., y)) (unapply i.0 a) ... (unapply i.n b) *)
}*)
(*
val prod : r ::: {Type*Type} -> folder r -> $(map (fn x => iso (fst x) (snd x)) r) -> iso $(map fst r) $(map snd r)
val sum : r ::: {Type} -> a ::: Type -> folder r -> $(map (fn x => iso x a) r) -> iso (variant r) a
*)
