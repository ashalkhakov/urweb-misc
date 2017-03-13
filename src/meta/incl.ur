(*
NOTE: taken verbatim from [meta] library!
*)

con incl' = K ==> fn (r1 :: {K}) (r2 :: {K}) (r' :: {K}) =>
                     [r1 ~ r'] => {Expose : f :: ({K} -> Type) -> f r2 -> f (r1 ++ r'),
                                   Hide : f :: ({K} -> Type) -> f (r1 ++ r') -> f r2}

con incl = K ==> fn (r1 :: {K}) (r2 :: {K}) =>
                    tp :: Type -> (r' :: {K} -> [r1 ~ r'] => incl' r1 r2 r' -> tp) -> tp

fun incl [K] [r1 :: {K}] [r2 :: {K}] [r1 ~ r2] =
 fn [tp :: Type] (f : r' :: {K} -> [r1 ~ r'] => incl' r1 (r1 ++ r2) r' -> tp) =>
    f [r2] (fn [r1 ~ r2] => {Expose = fn [f :: ({K} -> Type)] x => x,
                             Hide = fn [f :: ({K} -> Type)] x => x})

fun proj [r1 ::: {Type}] [r2 ::: {Type}] (i : incl r1 r2) (r : $r2) =
    i [$r1] (fn [r' :: {Type}] [r1 ~ r'] (i' : incl' r1 r2 r') =>
                i'.Expose [fn r => $r] r --- r')

(* pair two records together *)
fun zipR2 [tf1 :: (Type->Type)] [tf2 :: (Type->Type)] [tr :: ({Type}->Type)] [r ::: {Type}]
          (fl : folder r)
          (r1 : $(map tf1 r))
          (r2 : $(map tf2 r)) =
    @foldR2 [tf1] [tf2] [fn r => $(map (fn x => tf1 x * tf2 x) r)]
           (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest] x1 x2 rst =>
               {nm = (x1, x2)} ++ rst)
           {} fl r1 r2
    
(*fun proj0 [u ::: Type] [r1 ::: {Type}] [r2 ::: {Type}] [tf1 ::: Type->Type]
          (i : incl (map (fn _ => Unit) r1) (map (fn _ => Unit) r2))
          (fl : folder r1)
          (rec1: $r1) (rec2 : $(map tf1 r2)) =
    i [$(map (fn t => tf1 t * u) r1)]
      (fn [r' :: {Type}] [r1 ~ r']
                         (i' : incl' r1 r2 r') => let
            (* need to create a record with all sql_direction... *)
            val res = i'.Expose [fn r => $(map tf1 r)] rec2 --- r'
            val res = @zipR2 [fn x => tf1 x] [fn x => u] [fn r => $(map (fn x => u) r)] fl rec1 res
          in
            res
          end)*)

fun inv1 [K] [nm :: Name] [t :: K] [r :: {K}] [r' :: {K}] [[nm] ~ r]
         [f :: Name -> K -> {K} -> Type]
         (i : incl ([nm = t] ++ r) r')
         (f : nm :: Name -> t :: K -> r :: {K} -> [[nm] ~ r] => f nm t ([nm = t] ++ r)) =
    i [f nm t r'] (fn [r'' :: {K}] [[nm = t] ++ r ~ r''] (i' : incl' ([nm = t] ++ r) r' r'') =>
                      i'.Hide [f nm t] (f [nm] [t] [r ++ r'']))

fun inv2 [K] [nm :: Name] [t :: K] [r :: {K}] [r' :: {K}] [[nm] ~ r]
         (i : incl ([nm = t] ++ r) r') =
    i [incl r r'] (fn [r'' :: {K}] [[nm = t] ++ r ~ r''] (i' : incl' ([nm = t] ++ r) r' r'') =>
                   fn [tp :: Type] (f : r''' :: {K} -> [r ~ r'''] => incl' r r' r''' -> tp) =>
                      f [[nm = t] ++ r''] (fn [r ~ [nm = t] ++ r''] =>
                                              {Expose = fn [f :: ({K} -> Type)] (x : f r') => i'.Expose [f] x,
                                               Hide = fn [f :: ({K} -> Type)] x => i'.Hide [f] x}))

fun fold [K] [tf :: {K} -> Type] [r ::: {K}]
         (f : nm :: Name -> v :: K -> r' :: {K}
              -> [[nm] ~ r'] => incl ([nm = v] ++ r') r -> tf r' -> tf ([nm = v] ++ r'))
         (i : tf []) (fl : folder r) =
    @Top.fold [fn r' => incl r' r -> tf r']
     (fn [nm :: Name] [v :: K] [r' :: {K}] [[nm] ~ r'] acc i =>
         f [nm] [v] [r'] i (acc (inv2 [nm] [v] [r'] [r] i)))
     (fn _ => i)
     fl (incl [r] [[]])

(*fun mp [K] [r1 ::: {K}] [r2 ::: {K}] [tf :: (K -> Type)] (i : incl r1 r2) (fl : folder r2) =
    @fold [fn r => map tf r]
         (fn [nm ::_] [v ::_] [r' ::_] [[nm] ~ r'] (i0 : incl ([nm=v] ++ r') r) rst =>
             
         )
    (*_initial value??*) fl*)
