(* based on Andrew Kennedy's Pickler Combinators *)

type st s = (string * s)

con pu = fn (a :: Type) (p :: Type) => {Pickle : (a * st p) -> st p, Unpickle : st p -> (a * st p)}

type t = pu

fun pickle [a] [s] pu s x = (pu.Pickle (x, ("", s))).1 (* TODO reverse the output string first! *)
fun unpickle [a] [s] pu s x =  (pu.Unpickle (x, s)).1

fun lift [a ::: Type] [s ::: Type] (x : a) = {
    Pickle = fn (x : a, y : st s) => y,
    Unpickle = fn (s : st s) => (x,s)
}

fun sequ [a ::: Type] [b ::: Type] [s ::: Type] (f : b -> a) (pa : pu a s) (k : a -> pu b s) = {
  Pickle = fn (b, (cs, s)) => let
    val a = f b
    val pb = k a
    val (cs', s') = pa.Pickle (a, (cs, s))
    val (cs'', s'') = pb.Pickle (b, (cs', s'))
  in
    (cs'', s'')
  end,
  Unpickle = fn s => let
    val (a,s') = pa.Unpickle s
    val pa = k a
  in
    pa.Unpickle s'
  end
}

fun use_state [a] [s] (upd : a -> s -> s) (spa : s -> t a s) = {
  Pickle = fn (x, (cs, s)) => let
    val spap = spa s
    val (cs',s') = spap.Pickle (x, (cs, s))
  in
    (cs', upd x s')
  end,
  Unpickle = fn xcs => let
    val spap = spa (xcs.2)
    val (x, (cs', s')) = spap.Unpickle xcs
  in
    (x, (cs', upd x s'))
  end
}

fun wrap [a ::: Type] [b ::: Type] [s ::: Type] (i : a -> b) (j : b -> a) (pa : pu a s) =
    sequ j pa (compose lift i)

(* TODO test! PU.record {Hrs = PU.byte, Mins = PU.byte}
   what about skipping some constants?
   we basically need an isomorphism here...
   - {A = 5, B = 3} <-> {A = 5, C = 10, B = 3} (where C = 10 is constant, so we omit it)
   - can we selectively omit (or preserve) some record fields?
     - say given a record r ::: {Type}, we want to omit all fields of type unit
     - easy to do ad-hoc with the [wrap] combinator
 * TODO variant stuff too
    variant [r ::: {Type}] [a ::: Type] [s ::: Type]
            (_ : eq a)
            (x : $(map (fn x => PU.t x s) r)
            (a : PU.t a s)
            (f : a -> label)
	    (g : label -> a)
            -> PU.t (variant r) s
    read [x : pu a s] where [eq a]
    make your decision based on it: a -> label, label -> a
    pick the branch that you want to use: $(map (fn x => PU.t x s) r)
    the result will be put into the corresponding branch
 *)
fun record [r ::: {Type}] [s ::: Type] (x : $(map (fn x => pu x s) r)) (fl : folder r) =
  @@Top.foldR
     [fn x => pu x s] [fn r => pu $r s]
     (fn [nm :: Name] [a :: Type] [rest :: {Type}] [[nm] ~ rest]
		      (g : pu a s) (f : pu $rest s) =>
         sequ (fn (r : $([nm = a] ++ rest)) => r.nm) g
              (fn (g : a) =>
                  sequ (fn (r : $([nm = a] ++ rest)) => r -- nm)
		       f
		       (fn (f : $rest) => lift (f ++ {nm = g}))))
  (lift {}) [r] fl x

(*fun variant [r ::: {Unit}] [a ::: Type] -> s ::: Type
    (eq : eq a) (p : pu a s) (x : $(map (fn x => a -> pu x s) r)) (fl : folder r)
    sequ (fn (v : variant r) => let val (f, g) = match v x in ) p
    (fn (g : a) => _)
    (*
     read/write [a]
     then with the result:
	  - find the first matching branch
	  - if there is no match, fail
          - otherwise, you found the means of constructing/destructing b
          - 
*)
*)
